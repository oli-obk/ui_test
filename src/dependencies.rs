use cargo_metadata::{camino::Utf8PathBuf, DependencyKind};
use cargo_platform::Cfg;
use color_eyre::eyre::{bail, eyre, Result};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    ffi::OsString,
    path::PathBuf,
    process::Command,
    str::FromStr,
    sync::{Arc, OnceLock, RwLock},
};

use crate::{
    build_aux, status_emitter::StatusEmitter, Config, Errored, Mode, OutputConflictHandling,
};

#[derive(Default, Debug)]
pub struct Dependencies {
    /// All paths that must be imported with `-L dependency=`. This is for
    /// finding proc macros run on the host and dependencies for the target.
    pub import_paths: Vec<PathBuf>,
    /// The name as chosen in the `Cargo.toml` and its corresponding rmeta file.
    pub dependencies: Vec<(String, Vec<Utf8PathBuf>)>,
}

fn cfgs(config: &Config) -> Result<Vec<Cfg>> {
    let mut cmd = config.cfgs.build(&config.out_dir);
    cmd.arg("--target").arg(config.target.as_ref().unwrap());
    let output = cmd.output()?;
    let stdout = String::from_utf8(output.stdout)?;

    if !output.status.success() {
        let stderr = String::from_utf8(output.stderr)?;
        bail!(
            "failed to obtain `cfg` information from {cmd:?}:\nstderr:\n{stderr}\n\nstdout:{stdout}"
        );
    }
    let mut cfgs = vec![];

    for line in stdout.lines() {
        cfgs.push(Cfg::from_str(line)?);
    }

    Ok(cfgs)
}

/// Compiles dependencies and returns the crate names and corresponding rmeta files.
pub(crate) fn build_dependencies(config: &Config) -> Result<Dependencies> {
    let manifest_path = match &config.dependencies_crate_manifest_path {
        Some(path) => path.to_owned(),
        None => return Ok(Default::default()),
    };
    let manifest_path = &manifest_path;
    let mut build = config.dependency_builder.build(&config.out_dir);
    build.arg(manifest_path);

    if let Some(target) = &config.target {
        build.arg(format!("--target={target}"));
    }

    // Reusable closure for setting up the environment both for artifact generation and `cargo_metadata`
    let set_locking = |cmd: &mut Command| match (&config.output_conflict_handling, &config.mode) {
        (_, Mode::Yolo { .. }) => {}
        (OutputConflictHandling::Error(_), _) => {
            cmd.arg("--locked");
        }
        _ => {}
    };

    set_locking(&mut build);
    build.arg("--message-format=json");

    let output = build.output()?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        let stderr = String::from_utf8(output.stderr)?;
        bail!("failed to compile dependencies:\ncommand: {build:?}\nstderr:\n{stderr}\n\nstdout:{stdout}");
    }

    // Collect all artifacts generated
    let artifact_output = output.stdout;
    let artifact_output = String::from_utf8(artifact_output)?;
    let mut import_paths: HashSet<PathBuf> = HashSet::new();
    let mut artifacts = HashMap::new();
    for line in artifact_output.lines() {
        let Ok(message) = serde_json::from_str::<cargo_metadata::Message>(line) else {
            continue;
        };
        if let cargo_metadata::Message::CompilerArtifact(artifact) = message {
            if artifact
                .filenames
                .iter()
                .any(|f| f.ends_with("build-script-build"))
            {
                continue;
            }
            // Check that we only collect rmeta and rlib crates, not build script crates
            if artifact
                .filenames
                .iter()
                .any(|f| !matches!(f.extension(), Some("rlib" | "rmeta")))
            {
                continue;
            }
            for filename in &artifact.filenames {
                import_paths.insert(filename.parent().unwrap().into());
            }
            let package_id = artifact.package_id;
            if let Some(prev) = artifacts.insert(package_id.clone(), Ok(artifact.filenames)) {
                artifacts.insert(
                    package_id.clone(),
                    Err(format!("{prev:#?} vs {:#?}", artifacts[&package_id])),
                );
            }
        }
    }

    // Check which crates are mentioned in the crate itself
    let mut metadata = cargo_metadata::MetadataCommand::new().cargo_command();
    metadata.arg("--manifest-path").arg(manifest_path);
    config.dependency_builder.apply_env(&mut metadata);
    set_locking(&mut metadata);
    let output = metadata.output()?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        let stderr = String::from_utf8(output.stderr)?;
        bail!("failed to run cargo-metadata:\nstderr:\n{stderr}\n\nstdout:{stdout}");
    }

    let output = output.stdout;
    let output = String::from_utf8(output)?;

    let cfg = cfgs(config)?;

    for line in output.lines() {
        if !line.starts_with('{') {
            continue;
        }
        let metadata: cargo_metadata::Metadata = serde_json::from_str(line)?;
        // Only take artifacts that are defined in the Cargo.toml

        // First, find the root artifact
        let root = metadata
            .packages
            .iter()
            .find(|package| {
                package.manifest_path.as_std_path().canonicalize().unwrap()
                    == manifest_path.canonicalize().unwrap()
            })
            .unwrap();

        // Then go over all of its dependencies
        let dependencies = root
            .dependencies
            .iter()
            .filter(|dep| matches!(dep.kind, DependencyKind::Normal))
            // Only consider dependencies that are enabled on the current target
            .filter(|dep| match &dep.target {
                Some(platform) => platform.matches(config.target.as_ref().unwrap(), &cfg),
                None => true,
            })
            .map(|dep| {
                let package = metadata
                    .packages
                    .iter()
                    .find(|&p| p.name == dep.name && dep.req.matches(&p.version))
                    .expect("dependency does not exist");
                (
                    package,
                    dep.rename.clone().unwrap_or_else(|| package.name.clone()),
                )
            })
            // Also expose the root crate
            .chain(std::iter::once((root, root.name.clone())))
            .filter_map(|(package, name)| {
                // Get the id for the package matching the version requirement of the dep
                let id = &package.id;
                // Return the name chosen in `Cargo.toml` and the path to the corresponding artifact
                match artifacts.remove(id) {
                    Some(Ok(artifacts)) => Some(Ok((name.replace('-', "_"), artifacts))),
                    Some(Err(what)) => Some(Err(eyre!("`ui_test` does not support crates that appear as both build-dependencies and core dependencies: {id}: {what}"))),
                    None => {
                        if name == root.name {
                            // If there are no artifacts, this is the root crate and it is being built as a binary/test
                            // instead of a library. We simply add no artifacts, meaning you can't depend on functions
                            // and types declared in the root crate.
                            None
                        } else {
                            panic!("no artifact found for `{name}`(`{id}`):`\n{artifact_output}")
                        }
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;
        let import_paths = import_paths.into_iter().collect();
        return Ok(Dependencies {
            dependencies,
            import_paths,
        });
    }

    bail!("no json found in cargo-metadata output")
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Build {
    /// Build the dependencies.
    Dependencies,
    /// Build an aux-build.
    Aux { aux_file: PathBuf },
}
impl Build {
    fn description(&self) -> String {
        match self {
            Build::Dependencies => "Building dependencies".into(),
            Build::Aux { aux_file } => format!("Building aux file {}", aux_file.display()),
        }
    }
}

pub struct BuildManager<'a> {
    #[allow(clippy::type_complexity)]
    cache: RwLock<HashMap<Build, Arc<OnceLock<Result<Vec<OsString>, ()>>>>>,
    status_emitter: &'a dyn StatusEmitter,
}

impl<'a> BuildManager<'a> {
    pub fn new(status_emitter: &'a dyn StatusEmitter) -> Self {
        Self {
            cache: Default::default(),
            status_emitter,
        }
    }
    /// This function will block until the build is done and then return the arguments
    /// that need to be passed in order to build the dependencies.
    /// The error is only reported once, all follow up invocations of the same build will
    /// have a generic error about a previous build failing.
    pub fn build(&self, what: Build, config: &Config) -> Result<Vec<OsString>, Errored> {
        // Fast path without much contention.
        if let Some(res) = self.cache.read().unwrap().get(&what).and_then(|o| o.get()) {
            return res.clone().map_err(|()| Errored {
                command: Command::new(format!("{what:?}")),
                errors: vec![],
                stderr: b"previous build failed".to_vec(),
                stdout: vec![],
            });
        }
        let mut lock = self.cache.write().unwrap();
        let once = match lock.entry(what.clone()) {
            Entry::Occupied(entry) => {
                if let Some(res) = entry.get().get() {
                    return res.clone().map_err(|()| Errored {
                        command: Command::new(format!("{what:?}")),
                        errors: vec![],
                        stderr: b"previous build failed".to_vec(),
                        stdout: vec![],
                    });
                }
                entry.get().clone()
            }
            Entry::Vacant(entry) => {
                let once = Arc::new(OnceLock::new());
                entry.insert(once.clone());
                once
            }
        };
        drop(lock);

        let mut err = None;
        once.get_or_init(|| {
            let build = self
                .status_emitter
                .register_test(what.description().into())
                .for_revision("");
            let res = match &what {
                Build::Dependencies => match config.build_dependencies() {
                    Ok(args) => Ok(args),
                    Err(e) => {
                        err = Some(Errored {
                            command: Command::new(format!("{what:?}")),
                            errors: vec![],
                            stderr: format!("{e:?}").into_bytes(),
                            stdout: vec![],
                        });
                        Err(())
                    }
                },
                Build::Aux { aux_file } => match build_aux(aux_file, config, self) {
                    Ok(args) => Ok(args.iter().map(Into::into).collect()),
                    Err(e) => {
                        err = Some(e);
                        Err(())
                    }
                },
            };
            build.done(
                &res.as_ref()
                    .map(|_| crate::TestOk::Ok)
                    .map_err(|()| Errored {
                        command: Command::new(what.description()),
                        errors: vec![],
                        stderr: vec![],
                        stdout: vec![],
                    }),
            );
            res
        })
        .clone()
        .map_err(|()| {
            err.unwrap_or_else(|| Errored {
                command: Command::new(what.description()),
                errors: vec![],
                stderr: b"previous build failed".to_vec(),
                stdout: vec![],
            })
        })
    }
}
