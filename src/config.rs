use regex::bytes::Regex;

use crate::{dependencies::build_dependencies, CommandBuilder, Filter, Match, Mode, RustfixMode};
pub use color_eyre;
use color_eyre::eyre::Result;
use std::{
    ffi::OsString,
    num::NonZeroUsize,
    path::{Path, PathBuf},
};

mod args;
pub use args::{Args, Format};

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
    pub output_conflict_handling: OutputConflictHandling,
    /// Path to a `Cargo.toml` that describes which dependencies the tests can access.
    pub dependencies_crate_manifest_path: Option<PathBuf>,
    /// The command to run can be changed from `cargo` to any custom command to build the
    /// dependencies in `dependencies_crate_manifest_path`.
    pub dependency_builder: CommandBuilder,
    /// Where to dump files like the binaries compiled from tests.
    /// Defaults to `target/ui` in the current directory.
    pub out_dir: PathBuf,
    /// The default edition to use on all tests.
    pub edition: Option<String>,
    /// Skip test files whose names contain any of these entries.
    pub skip_files: Vec<String>,
    /// Only test files whose names contain any of these entries.
    pub filter_files: Vec<String>,
    /// Override the number of threads to use.
    pub threads: Option<NonZeroUsize>,
    /// Nextest emulation: only list the test itself, not its components.
    pub list: bool,
    /// Only run the tests that are ignored.
    pub run_only_ignored: bool,
    /// Filters must match exactly instead of just checking for substrings.
    pub filter_exact: bool,
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
                #[cfg(windows)]
                (Match::Exact(br"\\?\".to_vec()), b""),
            ],
            stdout_filters: vec![
                (Match::PathBackslash, b"/"),
                #[cfg(windows)]
                (Match::Exact(vec![b'\r']), b""),
                #[cfg(windows)]
                (Match::Exact(br"\\?\".to_vec()), b""),
            ],
            root_dir: root_dir.into(),
            mode: Mode::Fail {
                require_patterns: true,
                rustfix: RustfixMode::MachineApplicable,
            },
            program: CommandBuilder::rustc(),
            cfgs: CommandBuilder::cfgs(),
            output_conflict_handling: OutputConflictHandling::Bless,
            dependencies_crate_manifest_path: None,
            dependency_builder: CommandBuilder::cargo(),
            out_dir: std::env::var_os("CARGO_TARGET_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(|| std::env::current_dir().unwrap().join("target"))
                .join("ui"),
            edition: Some("2021".into()),
            skip_files: Vec::new(),
            filter_files: Vec::new(),
            threads: None,
            list: false,
            run_only_ignored: false,
            filter_exact: false,
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
                rustfix: RustfixMode::Disabled,
            },
            ..Self::rustc(root_dir)
        }
    }

    /// Populate the config with the values from parsed command line arguments.
    /// If neither `--bless` or `--check` are provided `default_bless` is used.
    ///
    /// The default output conflict handling command suggests adding `--bless`
    /// to the end of the current command.
    pub fn with_args(&mut self, args: &Args, default_bless: bool) {
        let Args {
            ref filters,
            check,
            bless,
            list,
            exact,
            ignored,
            format: _,
            threads,
            ref skip,
        } = *args;

        self.threads = threads.or(self.threads);

        self.filter_files.extend_from_slice(filters);
        self.skip_files.extend_from_slice(skip);
        self.run_only_ignored = ignored;
        self.filter_exact = exact;

        self.list = list;

        let bless = match (bless, check) {
            (_, true) => false,
            (true, _) => true,
            _ => default_bless,
        };
        self.output_conflict_handling = if bless {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error(format!(
                "{} --bless",
                std::env::args()
                    .map(|s| format!("{s:?}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ))
        };
    }

    /// Replace all occurrences of a path in stderr/stdout with a byte string.
    #[track_caller]
    pub fn path_filter(&mut self, path: &Path, replacement: &'static (impl AsRef<[u8]> + ?Sized)) {
        self.path_stderr_filter(path, replacement);
        self.path_stdout_filter(path, replacement);
    }

    /// Replace all occurrences of a path in stderr with a byte string.
    #[track_caller]
    pub fn path_stderr_filter(
        &mut self,
        path: &Path,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        let pattern = path.canonicalize().unwrap();
        self.stderr_filters
            .push((pattern.parent().unwrap().into(), replacement.as_ref()));
    }

    /// Replace all occurrences of a path in stdout with a byte string.
    #[track_caller]
    pub fn path_stdout_filter(
        &mut self,
        path: &Path,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        let pattern = path.canonicalize().unwrap();
        self.stdout_filters
            .push((pattern.parent().unwrap().into(), replacement.as_ref()));
    }

    /// Replace all occurrences of a regex pattern in stderr/stdout with a byte string.
    #[track_caller]
    pub fn filter(&mut self, pattern: &str, replacement: &'static (impl AsRef<[u8]> + ?Sized)) {
        self.stderr_filter(pattern, replacement);
        self.stdout_filter(pattern, replacement);
    }

    /// Replace all occurrences of a regex pattern in stderr with a byte string.
    #[track_caller]
    pub fn stderr_filter(
        &mut self,
        pattern: &str,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        self.stderr_filters
            .push((Regex::new(pattern).unwrap().into(), replacement.as_ref()));
    }

    /// Replace all occurrences of a regex pattern in stdout with a byte string.
    #[track_caller]
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
    /// The string should be a command that can be executed to bless all tests.
    Error(String),
    /// Ignore mismatches in the stderr/stdout files.
    Ignore,
    /// Instead of erroring if the stderr/stdout differs from the expected
    /// automatically replace it with the found output (after applying filters).
    Bless,
}
