#![allow(
    clippy::enum_variant_names,
    clippy::useless_format,
    clippy::too_many_arguments,
    rustc::internal
)]

use bstr::ByteSlice;
pub use color_eyre;
use color_eyre::eyre::Result;
use colored::*;
use crossbeam_channel::unbounded;
use parser::{ErrorMatch, Pattern};
use regex::bytes::Regex;
use rustc_stderr::{Level, Message};
use std::collections::VecDeque;
use std::ffi::OsString;
use std::fmt::Display;
use std::io::Write as _;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::thread;

use crate::dependencies::build_dependencies;
use crate::parser::{Comments, Condition};

mod dependencies;
mod diff;
mod parser;
mod rustc_stderr;
#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Config {
    /// Arguments passed to the binary that is executed.
    /// Take care to only append unless you actually meant to overwrite the defaults.
    /// Overwriting the defaults may make `//~ ERROR` style comments stop working.
    pub args: Vec<OsString>,
    /// Arguments passed to the binary that is executed.
    /// These arguments are passed *after* the args inserted via `//@compile-flags:`.
    pub trailing_args: Vec<OsString>,
    /// Host triple; usually will be auto-detected.
    pub host: Option<String>,
    /// `None` to run on the host, otherwise a target triple
    pub target: Option<String>,
    /// Filters applied to stderr output before processing it
    pub stderr_filters: Filter,
    /// Filters applied to stdout output before processing it
    pub stdout_filters: Filter,
    /// The folder in which to start searching for .rs files
    pub root_dir: PathBuf,
    pub mode: Mode,
    pub program: PathBuf,
    pub output_conflict_handling: OutputConflictHandling,
    /// Only run tests with one of these strings in their path/name
    pub path_filter: Vec<String>,
    /// Path to a `Cargo.toml` that describes which dependencies the tests can access.
    pub dependencies_crate_manifest_path: Option<PathBuf>,
    /// The command to run can be changed from `cargo` to any custom command to build the
    /// dependencies in `dependencies_crate_manifest_path`
    pub dependency_builder: DependencyBuilder,
    /// Print one character per test instead of one line
    pub quiet: bool,
    /// How many threads to use for running tests. Defaults to number of cores
    pub num_test_threads: NonZeroUsize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            args: vec!["--error-format=json".into()],
            trailing_args: vec![],
            host: None,
            target: None,
            stderr_filters: vec![],
            stdout_filters: vec![],
            root_dir: PathBuf::new(),
            mode: Mode::Fail {
                require_patterns: true,
            },
            program: PathBuf::from("rustc"),
            output_conflict_handling: OutputConflictHandling::Error,
            path_filter: vec![],
            dependencies_crate_manifest_path: None,
            dependency_builder: DependencyBuilder::default(),
            quiet: true,
            num_test_threads: std::thread::available_parallelism().unwrap(),
        }
    }
}

impl Config {
    pub fn stderr_filter(
        &mut self,
        pattern: &str,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        self.stderr_filters
            .push((Regex::new(pattern).unwrap(), replacement.as_ref()));
    }

    pub fn stdout_filter(
        &mut self,
        pattern: &str,
        replacement: &'static (impl AsRef<[u8]> + ?Sized),
    ) {
        self.stdout_filters
            .push((Regex::new(pattern).unwrap(), replacement.as_ref()));
    }

    fn build_dependencies_and_link_them(&mut self) -> Result<()> {
        let dependencies = build_dependencies(self)?;
        for (name, dependency) in dependencies.dependencies {
            self.args.push("--extern".into());
            let mut dep = OsString::from(name);
            dep.push("=");
            dep.push(dependency);
            self.args.push(dep);
        }
        for import_path in dependencies.import_paths {
            self.args.push("-L".into());
            self.args.push(import_path.into());
        }
        Ok(())
    }

    /// Make sure we have the host and target triples.
    pub fn fill_host_and_target(&mut self) {
        if self.host.is_none() {
            self.host = Some(
                rustc_version::VersionMeta::for_command(std::process::Command::new(&self.program))
                    .expect("failed to parse rustc version info")
                    .host,
            );
        }
        if self.target.is_none() {
            self.target = Some(self.host.clone().unwrap());
        }
    }
}

#[derive(Debug)]
pub struct DependencyBuilder {
    pub program: PathBuf,
    pub args: Vec<String>,
    pub envs: Vec<(String, OsString)>,
}

impl Default for DependencyBuilder {
    fn default() -> Self {
        Self {
            program: PathBuf::from(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
            args: vec!["build".into()],
            envs: vec![],
        }
    }
}

#[derive(Debug)]
pub enum OutputConflictHandling {
    /// The default: emit a diff of the expected/actual output.
    Error,
    /// Ignore mismatches in the stderr/stdout files.
    Ignore,
    /// Instead of erroring if the stderr/stdout differs from the expected
    /// automatically replace it with the found output (after applying filters).
    Bless,
}

pub type Filter = Vec<(Regex, &'static [u8])>;

pub fn run_tests(mut config: Config) -> Result<()> {
    eprintln!("   Compiler flags: {:?}", config.args);

    config.build_dependencies_and_link_them()?;

    run_tests_generic(config, |path| {
        path.extension().map(|ext| ext == "rs").unwrap_or(false)
    })
}

pub fn run_file(mut config: Config, path: &Path) -> Result<std::process::ExitStatus> {
    config.build_dependencies_and_link_them()?;

    let comments =
        Comments::parse_file(path)?.map_err(|errors| color_eyre::eyre::eyre!("{errors:#?}"))?;
    Ok(build_command(path, &config, "", &comments).status()?)
}

#[allow(clippy::large_enum_variant)]
enum TestResult {
    Ok,
    Ignored,
    Filtered,
    Errored {
        command: Command,
        errors: Vec<Error>,
        stderr: Vec<u8>,
    },
}

struct TestRun {
    result: TestResult,
    path: PathBuf,
    revision: String,
}

pub fn run_tests_generic(
    mut config: Config,
    file_filter: impl Fn(&Path) -> bool + Sync,
) -> Result<()> {
    config.fill_host_and_target();

    // A channel for files to process
    let (submit, receive) = unbounded();

    let mut results = vec![];

    thread::scope(|s| -> Result<()> {
        // Create a thread that is in charge of walking the directory and submitting jobs.
        // It closes the channel when it is done.
        s.spawn(|| {
            let mut todo = VecDeque::new();
            todo.push_back(config.root_dir.clone());
            while let Some(path) = todo.pop_front() {
                if path.is_dir() {
                    // Enqueue everything inside this directory.
                    // We want it sorted, to have some control over scheduling of slow tests.
                    let mut entries = std::fs::read_dir(path)
                        .unwrap()
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    entries.sort_by_key(|e| e.file_name());
                    for entry in entries {
                        todo.push_back(entry.path());
                    }
                } else if file_filter(&path) {
                    // Forward .rs files to the test workers.
                    submit.send(path).unwrap();
                }
            }
            // There will be no more jobs. This signals the workers to quit.
            // (This also ensures `submit` is moved into this closure.)
            drop(submit);
        });

        // A channel for the messages emitted by the individual test threads.
        // Used to produce live updates while running the tests.
        let (finished_files_sender, finished_files_recv) = unbounded::<TestRun>();

        s.spawn(|| {
            if config.quiet {
                for (i, run) in finished_files_recv.into_iter().enumerate() {
                    // Humans start counting at 1
                    let i = i + 1;
                    match run.result {
                        TestResult::Ok => eprint!("{}", ".".green()),
                        TestResult::Errored { .. } => eprint!("{}", "F".red().bold()),
                        TestResult::Ignored => eprint!("{}", "i".yellow()),
                        TestResult::Filtered => {}
                    }
                    if i % 100 == 0 {
                        eprintln!(" {i}");
                    }
                    results.push(run);
                }
            } else {
                for run in finished_files_recv {
                    let result = match run.result {
                        TestResult::Ok => Some("ok".green()),
                        TestResult::Errored { .. } => Some("FAILED".red().bold()),
                        TestResult::Ignored => Some("ignored (in-test comment)".yellow()),
                        TestResult::Filtered => None,
                    };
                    if let Some(result) = result {
                        eprint!(
                            "{}{} ... ",
                            run.path.display(),
                            if run.revision.is_empty() {
                                "".into()
                            } else {
                                format!(" ({})", run.revision)
                            }
                        );
                        eprintln!("{}", result);
                    }
                    results.push(run);
                }
            }
        });

        let mut threads = vec![];

        // Create N worker threads that receive files to test.
        for _ in 0..config.num_test_threads.get() {
            let finished_files_sender = finished_files_sender.clone();
            threads.push(s.spawn(|| -> Result<()> {
                let finished_files_sender = finished_files_sender;
                for path in &receive {
                    for result in parse_and_test_file(path, &config) {
                        finished_files_sender.send(result)?;
                    }
                }
                Ok(())
            }));
        }

        for thread in threads {
            thread.join().unwrap()?;
        }
        Ok(())
    })?;

    let mut failures = vec![];
    let mut succeeded = 0;
    let mut ignored = 0;
    let mut filtered = 0;

    for run in results {
        match run.result {
            TestResult::Ok => succeeded += 1,
            TestResult::Ignored => ignored += 1,
            TestResult::Filtered => filtered += 1,
            TestResult::Errored {
                command,
                errors,
                stderr,
            } => failures.push((run.path, command, run.revision, errors, stderr)),
        }
    }

    // Print all errors in a single thread to show reliable output
    if !failures.is_empty() {
        for (path, cmd, revision, errors, stderr) in &failures {
            eprintln!();
            eprint!("{}", path.display().to_string().underline().bold());
            if !revision.is_empty() {
                eprint!(" (revision `{}`)", revision);
            }
            eprint!(" {}", "FAILED:".red().bold());
            eprintln!();
            eprintln!("command: {:?}", cmd);
            eprintln!();
            for error in errors {
                match error {
                    Error::ExitStatus {
                        mode,
                        status,
                        expected,
                    } => {
                        eprintln!("{mode} test got {status}, but expected {expected}")
                    }
                    Error::PatternNotFound {
                        pattern,
                        definition_line,
                    } => {
                        match pattern {
                            Pattern::SubString(s) => {
                                eprintln!("substring `{s}` {} in stderr output", "not found".red())
                            }
                            Pattern::Regex(r) => {
                                eprintln!("`/{r}/` does {} stderr output", "not match".red())
                            }
                        }
                        eprintln!(
                            "expected because of pattern here: {}:{definition_line}",
                            path.display().to_string().bold()
                        );
                    }
                    Error::NoPatternsFound => {
                        eprintln!("{}", "no error patterns found in fail test".red());
                    }
                    Error::PatternFoundInPassTest => {
                        eprintln!("{}", "error pattern found in pass test".red())
                    }
                    Error::OutputDiffers {
                        path,
                        actual,
                        expected,
                    } => {
                        eprintln!("{}", "actual output differed from expected".underline());
                        eprintln!("{}", format!("--- {}", path.display()).red());
                        eprintln!("{}", "+++ <stderr output>".green());
                        diff::print_diff(expected, actual);
                    }
                    Error::ErrorsWithoutPattern { path: None, msgs } => {
                        eprintln!(
                            "There were {} unmatched diagnostics that occurred outside the testfile and had no pattern",
                            msgs.len(),
                        );
                        for Message { level, message } in msgs {
                            eprintln!("    {level:?}: {message}")
                        }
                    }
                    Error::ErrorsWithoutPattern {
                        path: Some((path, line)),
                        msgs,
                    } => {
                        eprintln!(
                            "There were {} unmatched diagnostics at {}:{line}",
                            msgs.len(),
                            path.display()
                        );
                        for Message { level, message } in msgs {
                            eprintln!("    {level:?}: {message}")
                        }
                    }
                    Error::InvalidComment { msg, line } => {
                        eprintln!(
                            "Could not parse comment in {}:{line} because {msg}",
                            path.display()
                        )
                    }
                }
                eprintln!();
            }
            eprintln!("full stderr:");
            std::io::stderr().write_all(stderr).unwrap();
            eprintln!();
            eprintln!();
        }
        eprintln!("{}", "FAILURES:".red().underline().bold());
        for (path, _cmd, _revision, _errors, _stderr) in &failures {
            eprintln!("    {}", path.display());
        }
        eprintln!();
        eprintln!(
            "test result: {}. {} tests failed, {} tests passed, {} ignored, {} filtered out",
            "FAIL".red(),
            failures.len().to_string().red().bold(),
            succeeded.to_string().green(),
            ignored.to_string().yellow(),
            filtered.to_string().yellow(),
        );
        std::process::exit(1);
    }
    eprintln!();
    eprintln!(
        "test result: {}. {} tests passed, {} ignored, {} filtered out",
        "ok".green(),
        succeeded.to_string().green(),
        ignored.to_string().yellow(),
        filtered.to_string().yellow(),
    );
    eprintln!();
    Ok(())
}

fn parse_and_test_file(path: PathBuf, config: &Config) -> Vec<TestRun> {
    if !config.path_filter.is_empty() {
        let path_display = path.display().to_string();
        if !config
            .path_filter
            .iter()
            .any(|filter| path_display.contains(filter))
        {
            return vec![TestRun {
                result: TestResult::Filtered,
                path,
                revision: "".into(),
            }];
        }
    }
    let comments = match Comments::parse_file(&path) {
        Ok(Ok(comments)) => comments,
        Ok(Err(errors)) => {
            return vec![TestRun {
                result: TestResult::Errored {
                    command: Command::new("parse comments"),
                    errors,
                    stderr: vec![],
                },
                path: path.clone(),
                revision: "".into(),
            }];
        }
        Err(err) => {
            return vec![TestRun {
                result: TestResult::Errored {
                    command: Command::new("parse comments"),
                    errors: vec![],
                    stderr: format!("{err:?}").into(),
                },
                path,
                revision: "".into(),
            }]
        }
    };
    // Run the test for all revisions
    comments
        .revisions
        .clone()
        .unwrap_or_else(|| vec![String::new()])
        .into_iter()
        .map(|revision| {
            // Ignore file if only/ignore rules do (not) apply
            if !test_file_conditions(&comments, config, &revision) {
                return TestRun {
                    result: TestResult::Ignored,
                    path: path.clone(),
                    revision,
                };
            }
            let (command, errors, stderr) = run_test(&path, config, &revision, &comments);
            let result = if errors.is_empty() {
                TestResult::Ok
            } else {
                TestResult::Errored {
                    command,
                    errors,
                    stderr,
                }
            };
            TestRun {
                result,
                revision,
                path: path.clone(),
            }
        })
        .collect()
}

#[derive(Debug)]
enum Error {
    /// Got an invalid exit status for the given mode.
    ExitStatus {
        mode: Mode,
        status: ExitStatus,
        expected: i32,
    },
    PatternNotFound {
        pattern: Pattern,
        definition_line: usize,
    },
    /// A ui test checking for failure does not have any failure patterns
    NoPatternsFound,
    /// A ui test checking for success has failure patterns
    PatternFoundInPassTest,
    /// Stderr/Stdout differed from the `.stderr`/`.stdout` file present.
    OutputDiffers {
        path: PathBuf,
        actual: Vec<u8>,
        expected: Vec<u8>,
    },
    ErrorsWithoutPattern {
        msgs: Vec<Message>,
        path: Option<(PathBuf, usize)>,
    },
    InvalidComment {
        msg: String,
        line: usize,
    },
}

type Errors = Vec<Error>;

fn build_command(path: &Path, config: &Config, revision: &str, comments: &Comments) -> Command {
    let mut cmd = Command::new(&config.program);
    cmd.args(config.args.iter());
    cmd.arg(path);
    if !revision.is_empty() {
        cmd.arg(format!("--cfg={revision}"));
    }
    for arg in comments
        .for_revision(revision)
        .flat_map(|r| r.compile_flags.iter())
    {
        cmd.arg(arg);
    }
    cmd.args(config.trailing_args.iter());
    cmd.envs(
        comments
            .for_revision(revision)
            .flat_map(|r| r.env_vars.iter())
            .map(|(k, v)| (k, v)),
    );

    cmd
}

fn run_test(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
) -> (Command, Errors, Vec<u8>) {
    let mut cmd = build_command(path, config, revision, comments);
    let output = cmd.output().expect("could not execute {cmd:?}");
    let mut errors = config.mode.ok(output.status);
    let stderr = check_test_result(
        path,
        config,
        revision,
        comments,
        &mut errors,
        &output.stdout,
        &output.stderr,
    );
    (cmd, errors, stderr)
}

fn check_test_result(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
    errors: &mut Errors,
    stdout: &[u8],
    stderr: &[u8],
) -> Vec<u8> {
    // Always remove annotation comments from stderr.
    let diagnostics = rustc_stderr::process(path, stderr);
    // Check output files (if any)
    let revised = |extension: &str| {
        if revision.is_empty() {
            extension.to_string()
        } else {
            format!("{}.{}", revision, extension)
        }
    };
    // Check output files against actual output
    check_output(
        &diagnostics.rendered,
        path,
        errors,
        revised("stderr"),
        &config.stderr_filters,
        config,
        comments,
        revision,
    );
    check_output(
        stdout,
        path,
        errors,
        revised("stdout"),
        &config.stdout_filters,
        config,
        comments,
        revision,
    );
    // Check error annotations in the source against output
    check_annotations(
        diagnostics.messages,
        diagnostics.messages_from_unknown_file_or_line,
        path,
        errors,
        config,
        revision,
        comments,
    );
    diagnostics.rendered
}

fn check_annotations(
    mut messages: Vec<Vec<Message>>,
    mut messages_from_unknown_file_or_line: Vec<Message>,
    path: &Path,
    errors: &mut Errors,
    config: &Config,
    revision: &str,
    comments: &Comments,
) {
    let error_pattern = comments.find_one_for_revision(
        revision,
        |r| r.error_pattern.as_ref(),
        |(_, line)| {
            errors.push(Error::InvalidComment {
                msg: "same revision defines pattern twice".into(),
                line: *line,
            })
        },
    );
    if let Some((error_pattern, definition_line)) = error_pattern {
        // first check the diagnostics messages outside of our file. We check this first, so that
        // you can mix in-file annotations with //@error-pattern annotations, even if there is overlap
        // in the messages.
        if let Some(i) = messages_from_unknown_file_or_line
            .iter()
            .position(|msg| error_pattern.matches(&msg.message))
        {
            messages_from_unknown_file_or_line.remove(i);
        } else {
            errors.push(Error::PatternNotFound {
                pattern: error_pattern.clone(),
                definition_line: *definition_line,
            });
        }
    }

    // The order on `Level` is such that `Error` is the highest level.
    // We will ensure that *all* diagnostics of level at least `lowest_annotation_level`
    // are matched.
    let mut lowest_annotation_level = Level::Error;
    let mut seen_error_match = false;
    for &ErrorMatch {
        ref pattern,
        definition_line,
        line,
        level,
    } in comments
        .for_revision(revision)
        .flat_map(|r| r.error_matches.iter())
    {
        seen_error_match = true;
        // If we found a diagnostic with a level annotation, make sure that all
        // diagnostics of that level have annotations, even if we don't end up finding a matching diagnostic
        // for this pattern.
        lowest_annotation_level = std::cmp::min(lowest_annotation_level, level);

        if let Some(msgs) = messages.get_mut(line) {
            let found = msgs
                .iter()
                .position(|msg| pattern.matches(&msg.message) && msg.level == level);
            if let Some(found) = found {
                msgs.remove(found);
                continue;
            }
        }

        errors.push(Error::PatternNotFound {
            pattern: pattern.clone(),
            definition_line,
        });
    }

    let filter = |mut msgs: Vec<Message>, errors: &mut Vec<_>| -> Vec<_> {
        let error = |_| {
            errors.push(Error::InvalidComment {
                msg: "`require_annotations_for_level` specified twice for same revision".into(),
                line: 0,
            })
        };
        let required_annotation_level = comments
            .find_one_for_revision(revision, |r| r.require_annotations_for_level, error)
            .unwrap_or(lowest_annotation_level);
        msgs.retain(|msg| msg.level >= required_annotation_level);
        msgs
    };

    let messages_from_unknown_file_or_line = filter(messages_from_unknown_file_or_line, errors);
    if !messages_from_unknown_file_or_line.is_empty() {
        errors.push(Error::ErrorsWithoutPattern {
            path: None,
            msgs: messages_from_unknown_file_or_line,
        });
    }

    for (line, msgs) in messages.into_iter().enumerate() {
        let msgs = filter(msgs, errors);
        if !msgs.is_empty() {
            errors.push(Error::ErrorsWithoutPattern {
                path: Some((path.to_path_buf(), line)),
                msgs,
            });
        }
    }

    match (config.mode, error_pattern.is_some() || seen_error_match) {
        (Mode::Pass, true) | (Mode::Panic, true) => errors.push(Error::PatternFoundInPassTest),
        (
            Mode::Fail {
                require_patterns: true,
            },
            false,
        ) => errors.push(Error::NoPatternsFound),
        _ => {}
    }
}

fn check_output(
    output: &[u8],
    path: &Path,
    errors: &mut Errors,
    kind: String,
    filters: &Filter,
    config: &Config,
    comments: &Comments,
    revision: &str,
) {
    let target = config.target.as_ref().unwrap();
    let output = normalize(path, output, filters, comments, revision);
    let path = output_path(path, comments, kind, target, revision);
    match config.output_conflict_handling {
        OutputConflictHandling::Bless => {
            if output.is_empty() {
                let _ = std::fs::remove_file(path);
            } else {
                std::fs::write(path, &output).unwrap();
            }
        }
        OutputConflictHandling::Error => {
            let expected_output = std::fs::read(&path).unwrap_or_default();
            if output != expected_output {
                errors.push(Error::OutputDiffers {
                    path,
                    actual: output,
                    expected: expected_output,
                });
            }
        }
        OutputConflictHandling::Ignore => {}
    }
}

fn output_path(
    path: &Path,
    comments: &Comments,
    kind: String,
    target: &str,
    revision: &str,
) -> PathBuf {
    if comments
        .for_revision(revision)
        .any(|r| r.stderr_per_bitwidth)
    {
        return path.with_extension(format!("{}bit.{kind}", get_pointer_width(target)));
    }
    path.with_extension(kind)
}

fn test_condition(condition: &Condition, config: &Config) -> bool {
    let target = config.target.as_ref().unwrap();
    match condition {
        Condition::Bitwidth(bits) => get_pointer_width(target) == *bits,
        Condition::Target(t) => target.contains(t),
        Condition::OnHost => target == config.host.as_ref().unwrap(),
    }
}

/// Returns whether according to the in-file conditions, this file should be run.
fn test_file_conditions(comments: &Comments, config: &Config, revision: &str) -> bool {
    if comments
        .for_revision(revision)
        .flat_map(|r| r.ignore.iter())
        .any(|c| test_condition(c, config))
    {
        return false;
    }
    comments
        .for_revision(revision)
        .flat_map(|r| r.only.iter())
        .all(|c| test_condition(c, config))
}

// Taken 1:1 from compiletest-rs
fn get_pointer_width(triple: &str) -> u8 {
    if (triple.contains("64") && !triple.ends_with("gnux32") && !triple.ends_with("gnu_ilp32"))
        || triple.starts_with("s390x")
    {
        64
    } else if triple.starts_with("avr") {
        16
    } else {
        32
    }
}

fn normalize(
    path: &Path,
    text: &[u8],
    filters: &Filter,
    comments: &Comments,
    revision: &str,
) -> Vec<u8> {
    // Useless paths
    let mut text = text.replace(&path.parent().unwrap().display().to_string(), "$DIR");
    if let Some(lib_path) = option_env!("RUSTC_LIB_PATH") {
        text = text.replace(lib_path, "RUSTLIB");
    }

    for (regex, replacement) in filters.iter() {
        text = regex.replace_all(&text, *replacement).into_owned();
    }

    for (from, to) in comments
        .for_revision(revision)
        .flat_map(|r| r.normalize_stderr.iter())
    {
        text = from.replace_all(&text, to).into_owned();
    }
    text
}

#[derive(Copy, Clone, Debug)]
pub enum Mode {
    /// The test passes a full execution of the rustc driver
    Pass,
    /// The rustc driver panicked
    Panic,
    /// The rustc driver emitted an error
    Fail {
        /// Whether failing tests must have error patterns. Set to false if you just care about .stderr output.
        require_patterns: bool,
    },
}

impl Mode {
    fn ok(self, status: ExitStatus) -> Errors {
        let expected = match self {
            Mode::Pass => 0,
            Mode::Panic => 101,
            Mode::Fail { .. } => 1,
        };
        if status.code() == Some(expected) {
            vec![]
        } else {
            vec![Error::ExitStatus {
                mode: self,
                status,
                expected,
            }]
        }
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Pass => write!(f, "pass"),
            Mode::Panic => write!(f, "panic"),
            Mode::Fail {
                require_patterns: _,
            } => write!(f, "fail"),
        }
    }
}
