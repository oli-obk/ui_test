//! Use `cargo` to build dependencies and make them available in your tests

use cargo_metadata::{camino::Utf8PathBuf, BuildScript, DependencyKind};
use cargo_platform::Cfg;
use color_eyre::eyre::{bail, eyre, Result};
use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    path::PathBuf,
    process::Command,
    str::FromStr,
};

use crate::{
    build_manager::{Build, BuildManager},
    custom_flags::Flag,
    per_test_config::TestConfig,
    test_result::Errored,
    CommandBuilder, Config, Mode, OutputConflictHandling,
};

#[derive(Default, Debug)]
/// Describes where to find the binaries built for the dependencies
pub struct Dependencies {
    /// All paths that must be imported with `-L dependency=`. This is for
    /// finding proc macros run on the host and dependencies for the target.
    pub import_paths: Vec<PathBuf>,
    /// Unnamed dependencies that build scripts asked us to link
    pub import_libs: Vec<PathBuf>,
    /// The name as chosen in the `Cargo.toml` and its corresponding rmeta file.
    pub dependencies: Vec<(String, Vec<Utf8PathBuf>)>,
}

fn cfgs(config: &Config) -> Result<Vec<Cfg>> {
    let Some(cfg) = &config.program.cfg_flag else {
        return Ok(vec![]);
    };
    let mut cmd = config.program.build(&config.out_dir);
    cmd.arg(cfg);
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
fn build_dependencies_inner(config: &Config, info: &DependencyBuilder) -> Result<Dependencies> {
    let mut build = info.program.build(&config.out_dir);
    build.arg(&info.crate_manifest_path);

    if let Some(target) = &config.target {
        build.arg(format!("--target={target}"));
    }

    // Reusable closure for setting up the environment both for artifact generation and `cargo_metadata`
    let set_locking = |cmd: &mut Command| match (
        &config.output_conflict_handling,
        config
            .comment_defaults
            .base_immut()
            .mode
            .as_deref()
            .unwrap(),
    ) {
        (_, Mode::Yolo { .. }) => {}
        (OutputConflictHandling::Error, _) => {
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
    let mut import_libs: HashSet<PathBuf> = HashSet::new();
    let mut artifacts = HashMap::new();
    'artifact: for line in artifact_output.lines() {
        let Ok(message) = serde_json::from_str::<cargo_metadata::Message>(line) else {
            continue;
        };
        match message {
            cargo_metadata::Message::CompilerArtifact(artifact) => {
                for ctype in &artifact.target.crate_types {
                    match ctype.as_str() {
                        "proc-macro" | "lib" => {}
                        _ => continue 'artifact,
                    }
                }
                for filename in &artifact.filenames {
                    import_paths.insert(filename.parent().unwrap().into());
                }
                let package_id = artifact.package_id;
                if let Some(prev) = artifacts.insert(package_id.clone(), Ok(artifact.filenames)) {
                    artifacts.insert(
                        package_id.clone(),
                        Err(format!(
                            "{prev:#?} vs {:#?} ({:?})",
                            artifacts[&package_id], artifact.target.crate_types
                        )),
                    );
                }
            }
            cargo_metadata::Message::BuildScriptExecuted(BuildScript {
                linked_libs,
                linked_paths,
                ..
            }) => {
                import_paths.extend(linked_paths.into_iter().map(Into::into));
                import_libs.extend(linked_libs.into_iter().map(Into::into));
            }
            _ => {}
        }
    }

    // Check which crates are mentioned in the crate itself
    let mut metadata = cargo_metadata::MetadataCommand::new().cargo_command();
    metadata
        .arg("--manifest-path")
        .arg(&info.crate_manifest_path);
    info.program.apply_env(&mut metadata);
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
                    == info.crate_manifest_path.canonicalize().unwrap()
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
                for p in &metadata.packages {
                    if p.name != dep.name {
                        continue;
                    }
                    if dep.path.as_ref().is_some_and(|path| p.manifest_path.parent().unwrap() == path) || dep.req.matches(&p.version) {
                        return (
                            p,
                            dep.rename.clone().unwrap_or_else(|| p.name.clone()),
                        )
                    }
                }
                panic!("dep not found: {dep:#?}")
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
        let import_libs = import_libs.into_iter().collect();
        return Ok(Dependencies {
            dependencies,
            import_paths,
            import_libs,
        });
    }

    bail!("no json found in cargo-metadata output")
}

/// Build the dependencies.
#[derive(Debug, Clone)]
pub struct DependencyBuilder {
    /// Path to a `Cargo.toml` that describes which dependencies the tests can access.
    pub crate_manifest_path: PathBuf,
    /// The command to run can be changed from `cargo` to any custom command to build the
    /// dependencies in `crate_manifest_path`.
    pub program: CommandBuilder,
}

impl Default for DependencyBuilder {
    fn default() -> Self {
        Self {
            crate_manifest_path: PathBuf::from("Cargo.toml"),
            program: CommandBuilder::cargo(),
        }
    }
}

impl Flag for DependencyBuilder {
    fn must_be_unique(&self) -> bool {
        true
    }
    fn clone_inner(&self) -> Box<dyn Flag> {
        Box::new(self.clone())
    }
    fn apply(
        &self,
        cmd: &mut Command,
        config: &TestConfig<'_>,
        build_manager: &BuildManager<'_>,
    ) -> Result<(), Errored> {
        config
            .status
            .update_status("waiting for dependencies to finish building".into());
        let extra_args = build_manager.build(self.clone())?;
        cmd.args(extra_args);
        config.status.update_status(String::new());
        Ok(())
    }
}

impl Build for DependencyBuilder {
    fn build(&self, build_manager: &BuildManager<'_>) -> Result<Vec<OsString>, Errored> {
        build_dependencies(build_manager.config(), self).map_err(|e| Errored {
            command: Command::new(format!("{:?}", self.description())),
            errors: vec![],
            stderr: format!("{e:?}").into_bytes(),
            stdout: vec![],
        })
    }

    fn description(&self) -> String {
        "Building dependencies".into()
    }
}

/// Compile dependencies and return the right flags
/// to find the dependencies.
pub fn build_dependencies(config: &Config, info: &DependencyBuilder) -> Result<Vec<OsString>> {
    let dependencies = build_dependencies_inner(config, info)?;
    let mut args = vec![];
    for (name, artifacts) in dependencies.dependencies {
        for dependency in artifacts {
            args.push("--extern".into());
            let mut dep = OsString::from(&name);
            dep.push("=");
            dep.push(dependency);
            args.push(dep);
        }
    }
    for import_path in dependencies.import_paths {
        args.push("-L".into());
        args.push(import_path.into());
    }
    for import_path in dependencies.import_libs {
        args.push("-l".into());
        args.push(import_path.into());
    }
    Ok(args)
}
