use cargo_metadata::{camino::Utf8PathBuf, DependencyKind};
use color_eyre::eyre::{bail, Result};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    process::Command,
};

use crate::{Config, DependencyBuilder, OutputConflictHandling};

#[derive(Default, Debug)]
pub struct Dependencies {
    /// All paths that must be imported with `-L dependency=`. This is for
    /// finding proc macros run on the host and dependencies for the target.
    pub import_paths: Vec<PathBuf>,
    /// The name as chosen in the `Cargo.toml` and its corresponding rmeta file.
    pub dependencies: Vec<(String, Vec<Utf8PathBuf>)>,
}

/// Compiles dependencies and returns the crate names and corresponding rmeta files.
pub fn build_dependencies(config: &Config) -> Result<Dependencies> {
    let manifest_path = match &config.dependencies_crate_manifest_path {
        Some(path) => path,
        None => return Ok(Default::default()),
    };
    eprintln!("   Building test dependencies...");
    let DependencyBuilder {
        program,
        args,
        envs,
    } = &config.dependency_builder;
    let mut build = Command::new(program);
    build.args(args);

    if let Some(target) = &config.target {
        build.arg(format!("--target={target}"));
    }

    // Reusable closure for setting up the environment both for artifact generation and `cargo_metadata`
    let setup_command = |cmd: &mut Command| {
        cmd.envs(envs.iter().map(|(k, v)| (k, v)));
        cmd.arg("--manifest-path").arg(manifest_path);
        if matches!(
            config.output_conflict_handling,
            OutputConflictHandling::Error
        ) {
            cmd.arg("--locked");
        }
    };

    setup_command(&mut build);
    build.arg("--message-format=json");

    let output = build.output()?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        let stderr = String::from_utf8(output.stderr)?;
        bail!("failed to compile dependencies:\nstderr:\n{stderr}\n\nstdout:{stdout}");
    }

    // Collect all artifacts generated
    let artifact_output = output.stdout;
    let artifact_output = String::from_utf8(artifact_output)?;
    let mut import_paths: HashSet<PathBuf> = HashSet::new();
    let mut artifacts: HashMap<_, _> = artifact_output
        .lines()
        .filter_map(|line| {
            let message = serde_json::from_str::<cargo_metadata::Message>(line).ok()?;
            if let cargo_metadata::Message::CompilerArtifact(artifact) = message {
                for filename in &artifact.filenames {
                    import_paths.insert(filename.parent().unwrap().into());
                }
                Some((artifact.package_id, artifact.filenames))
            } else {
                None
            }
        })
        .collect();

    // Check which crates are mentioned in the crate itself
    let mut metadata = cargo_metadata::MetadataCommand::new().cargo_command();
    setup_command(&mut metadata);
    let output = metadata.output()?;

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        let stderr = String::from_utf8(output.stderr)?;
        bail!("failed to run cargo-metadata:\nstderr:\n{stderr}\n\nstdout:{stdout}");
    }

    let output = output.stdout;
    let output = String::from_utf8(output)?;

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
                    Some(artifacts) => Some((name.replace('-', "_"), artifacts)),
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
            .collect();
        let import_paths = import_paths.into_iter().collect();
        return Ok(Dependencies {
            dependencies,
            import_paths,
        });
    }

    bail!("no json found in cargo-metadata output")
}
