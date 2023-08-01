use regex::bytes::Regex;

use crate::{dependencies::build_dependencies, CommandBuilder, Filter, Match, Mode};
pub use color_eyre;
use color_eyre::eyre::Result;
use std::{
    ffi::OsString,
    num::NonZeroUsize,
    path::{Path, PathBuf},
};

mod args;
pub use args::Args;

#[derive(Debug, Clone)]
/// Central datastructure containing all information to run the tests.
pub struct Config {
    /// Host triple; usually will be auto-detected.
    pub host: Option<String>,
    /// `None` to run on the host, otherwise a target triple
    pub target: Option<String>,
    /// Filters applied to stderr output before processing it.
    /// By default contains a filter for replacing backslashes in paths with
    /// regular slashes.
    /// On windows, contains a filter to remove `\r`.
    pub stderr_filters: Filter,
    /// Filters applied to stdout output before processing it.
    /// On windows, contains a filter to remove `\r`.
    pub stdout_filters: Filter,
    /// The folder in which to start searching for .rs files
    pub root_dir: PathBuf,
    /// The mode in which to run the tests.
    pub mode: Mode,
    /// The binary to actually execute.
    pub program: CommandBuilder,
    /// The command to run to obtain the cfgs that the output is supposed to
    pub cfgs: CommandBuilder,
    /// What to do in case the stdout/stderr output differs from the expected one.
    /// By default, errors in case of conflict, but emits a message informing the user
    /// that running `cargo test -- -- --bless` will automatically overwrite the
    /// `.stdout` and `.stderr` files with the latest output.
    pub output_conflict_handling: OutputConflictHandling,
    /// Path to a `Cargo.toml` that describes which dependencies the tests can access.
    pub dependencies_crate_manifest_path: Option<PathBuf>,
    /// The command to run can be changed from `cargo` to any custom command to build the
    /// dependencies in `dependencies_crate_manifest_path`
    pub dependency_builder: CommandBuilder,
    /// How many threads to use for running tests. Defaults to number of cores
    pub num_test_threads: NonZeroUsize,
    /// Where to dump files like the binaries compiled from tests.
    /// Defaults to `target/ui` in the current directory.
    pub out_dir: PathBuf,
    /// The default edition to use on all tests
    pub edition: Option<String>,
}

impl Config {
    /// Create a configuration for testing the output of running
    /// `rustc` on the test files.
    pub fn rustc(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            host: None,
            target: None,
            stderr_filters: vec![
                (Match::PathBackslash, b"/"),
                #[cfg(windows)]
                (Match::Exact(vec![b'\r']), b""),
            ],
            stdout_filters: vec![
                #[cfg(windows)]
                (Match::Exact(vec![b'\r']), b""),
            ],
            root_dir: root_dir.into(),
            mode: Mode::Fail {
                require_patterns: true,
                rustfix: true,
            },
            program: CommandBuilder::rustc(),
            cfgs: CommandBuilder::cfgs(),
            output_conflict_handling: OutputConflictHandling::Error(format!(
                "{} --bless",
                std::env::args()
                    .map(|s| format!("{s:?}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            )),
            dependencies_crate_manifest_path: None,
            dependency_builder: CommandBuilder::cargo(),
            num_test_threads: std::thread::available_parallelism().unwrap(),
            out_dir: std::env::var_os("CARGO_TARGET_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|| std::env::current_dir().unwrap().join("target"))
                .join("ui"),
            edition: Some("2021".into()),
        }
    }

    /// Create a configuration for testing the output of running
    /// `cargo` on the test `Cargo.toml` files.
    pub fn cargo(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            program: CommandBuilder::cargo(),
            edition: None,
            mode: Mode::Fail {
                require_patterns: true,
                rustfix: false,
            },
            ..Self::rustc(root_dir)
        }
    }

    /// Replace all occurrences of a path in stderr with a byte string.
    pub fn path_stderr_filter(
        &mut self,
        path: &Path,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        let pattern = path.canonicalize().unwrap();
        self.stderr_filters
            .push((pattern.parent().unwrap().into(), replacement.as_ref()));
    }

    /// Replace all occurrences of a regex pattern in stderr with a byte string.
    pub fn stderr_filter(
        &mut self,
        pattern: &str,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        self.stderr_filters
            .push((Regex::new(pattern).unwrap().into(), replacement.as_ref()));
    }

    /// Replace all occurrences of a regex pattern in stdout with a byte string.
    pub fn stdout_filter(
        &mut self,
        pattern: &str,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        self.stdout_filters
            .push((Regex::new(pattern).unwrap().into(), replacement.as_ref()));
    }

    /// Compile dependencies and return the right flags
    /// to find the dependencies.
    pub fn build_dependencies(&self) -> Result<Vec<OsString>> {
        let dependencies = build_dependencies(self)?;
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
        Ok(args)
    }

    /// Make sure we have the host and target triples.
    pub fn fill_host_and_target(&mut self) -> Result<()> {
        if self.host.is_none() {
            self.host = Some(
                rustc_version::VersionMeta::for_command(std::process::Command::new(
                    &self.program.program,
                ))
                .map_err(|err| {
                    color_eyre::eyre::Report::new(err).wrap_err(format!(
                        "failed to parse rustc version info: {}",
                        self.program.display()
                    ))
                })?
                .host,
            );
        }
        if self.target.is_none() {
            self.target = Some(self.host.clone().unwrap());
        }
        Ok(())
    }

    pub(crate) fn has_asm_support(&self) -> bool {
        static ASM_SUPPORTED_ARCHS: &[&str] = &[
            "x86", "x86_64", "arm", "aarch64", "riscv32",
            "riscv64",
            // These targets require an additional asm_experimental_arch feature.
            // "nvptx64", "hexagon", "mips", "mips64", "spirv", "wasm32",
        ];
        ASM_SUPPORTED_ARCHS
            .iter()
            .any(|arch| self.target.as_ref().unwrap().contains(arch))
    }
}

#[derive(Debug, Clone)]
/// The different options for what to do when stdout/stderr files differ from the actual output.
pub enum OutputConflictHandling {
    /// The default: emit a diff of the expected/actual output.
    ///
    /// The string should be a command that can be executed to bless all tests.
    Error(String),
    /// Ignore mismatches in the stderr/stdout files.
    Ignore,
    /// Instead of erroring if the stderr/stdout differs from the expected
    /// automatically replace it with the found output (after applying filters).
    Bless,
}
