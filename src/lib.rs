#![allow(
    clippy::enum_variant_names,
    clippy::useless_format,
    clippy::too_many_arguments,
    rustc::internal
)]
#![deny(missing_docs)]

//! A crate to run the Rust compiler (or other binaries) and test their command line output.

use bstr::ByteSlice;
pub use clap;
use clap::Parser;
pub use color_eyre;
use color_eyre::eyre::{eyre, Result};
use crossbeam_channel::{unbounded, Receiver, Sender};
use dependencies::{Build, BuildManager};
use lazy_static::lazy_static;
use parser::{ErrorMatch, MaybeWithLine, OptWithLine, Revisioned, WithLine};
use read_helper::ReadHelper;
use regex::bytes::{Captures, Regex};
use rustc_stderr::{Level, Message};
use status_emitter::{StatusEmitter, TestStatus};
use std::borrow::Cow;
use std::collections::{HashSet, VecDeque};
use std::ffi::OsString;
use std::num::NonZeroUsize;
use std::path::{Component, Path, PathBuf, Prefix};
use std::process::{Command, ExitStatus, Stdio};
use std::thread;
use std::time::Duration;

use crate::parser::{Comments, Condition};

mod cmd;
mod config;
mod dependencies;
mod diff;
mod error;
pub mod github_actions;
mod mode;
mod parser;
mod read_helper;
mod rustc_stderr;
pub mod status_emitter;
#[cfg(test)]
mod tests;

pub use cmd::*;
pub use config::*;
pub use error::*;
pub use mode::*;

/// A filter's match rule.
#[derive(Clone, Debug)]
pub enum Match {
    /// If the regex matches, the filter applies
    Regex(Regex),
    /// If the exact byte sequence is found, the filter applies
    Exact(Vec<u8>),
    /// Uses a heuristic to find backslashes in windows style paths
    PathBackslash,
}
impl Match {
    fn replace_all<'a>(&self, text: &'a [u8], replacement: &[u8]) -> Cow<'a, [u8]> {
        match self {
            Match::Regex(regex) => regex.replace_all(text, replacement),
            Match::Exact(needle) => text.replace(needle, replacement).into(),
            Match::PathBackslash => {
                lazy_static! {
                    static ref PATH_RE: Regex = Regex::new(
                        r"(?x)
                        (?:
                            # Match paths to files with extensions that don't include spaces
                            \\(?:[\pL\pN.\-_']+[/\\])*[\pL\pN.\-_']+\.\pL+
                        |
                            # Allow spaces in absolute paths
                            [A-Z]:\\(?:[\pL\pN.\-_'\ ]+[/\\])+
                        )",
                    )
                    .unwrap();
                }

                PATH_RE.replace_all(text, |caps: &Captures<'_>| {
                    caps[0].replace(r"\", replacement)
                })
            }
        }
    }
}

impl From<&'_ Path> for Match {
    fn from(v: &Path) -> Self {
        let mut v = v.display().to_string();
        // Normalize away windows canonicalized paths.
        if v.starts_with(r"\\?\") {
            v.drain(0..4);
        }
        let mut v = v.into_bytes();
        // Normalize paths on windows to use slashes instead of backslashes,
        // So that paths are rendered the same on all systems.
        for c in &mut v {
            if *c == b'\\' {
                *c = b'/';
            }
        }
        Self::Exact(v)
    }
}

impl From<Regex> for Match {
    fn from(v: Regex) -> Self {
        Self::Regex(v)
    }
}

/// Replacements to apply to output files.
pub type Filter = Vec<(Match, &'static [u8])>;

/// Run all tests as described in the config argument.
/// Will additionally process command line arguments.
pub fn run_tests(config: Config) -> Result<()> {
    let args = Args::parse();
    if !args.quiet {
        eprintln!("   Compiler: {}", config.program.display());
    }

    let name = config.root_dir.display().to_string();

    let text = if args.quiet {
        status_emitter::Text::quiet()
    } else {
        status_emitter::Text::verbose()
    };

    run_tests_generic(
        config,
        args,
        default_file_filter,
        default_per_file_config,
        (text, status_emitter::Gha::<true> { name }),
    )
}

/// The filter used by `run_tests` to only run on `.rs` files
/// and those specified in the command line args.
pub fn default_file_filter(path: &Path, args: &Args) -> bool {
    path.extension().is_some_and(|ext| ext == "rs") && default_filter_by_arg(path, args)
}

/// Run on all files that are matched by the filter in the argument list
pub fn default_filter_by_arg(path: &Path, args: &Args) -> bool {
    if args.filters.is_empty() {
        return true;
    }
    let path = path.display().to_string();
    args.filters.iter().any(|f| path.contains(f))
}

/// The default per-file config used by `run_tests`.
pub fn default_per_file_config(config: &Config, path: &Path) -> Option<Config> {
    let mut config = config.clone();
    // Heuristic:
    // * if the file contains `#[test]`, automatically pass `--cfg test`.
    // * if the file does not contain `fn main()` or `#[start]`, automatically pass `--crate-type=lib`.
    // This avoids having to spam `fn main() {}` in almost every test.
    let file_contents = std::fs::read(path).unwrap();
    if file_contents.find(b"#[proc_macro").is_some() {
        config.program.args.push("--crate-type=proc-macro".into())
    } else if file_contents.find(b"#[test]").is_some() {
        config.program.args.push("--test".into());
    } else if file_contents.find(b"fn main()").is_none()
        && file_contents.find(b"#[start]").is_none()
    {
        config.program.args.push("--crate-type=lib".into());
    }
    Some(config)
}

/// Create a command for running a single file, with the settings from the `config` argument.
/// Ignores various settings from `Config` that relate to finding test files.
pub fn test_command(mut config: Config, path: &Path) -> Result<Command> {
    config.fill_host_and_target()?;
    let extra_args = config.build_dependencies()?;

    let comments =
        Comments::parse_file(path)?.map_err(|errors| color_eyre::eyre::eyre!("{errors:#?}"))?;
    let mut result = build_command(path, &config, "", &comments).unwrap();
    result.args(extra_args);

    Ok(result)
}

/// The possible non-failure results a single test can have.
pub enum TestOk {
    /// The test passed
    Ok,
    /// The test was ignored due to a rule (`//@only-*` or `//@ignore-*`)
    Ignored,
    /// The test was filtered with the `file_filter` argument.
    Filtered,
}

/// The possible results a single test can have.
pub type TestResult = Result<TestOk, Errored>;

/// Information about a test failure.
#[derive(Debug)]
pub struct Errored {
    /// Command that failed
    command: Command,
    /// The errors that were encountered.
    errors: Vec<Error>,
    /// The full stderr of the test run.
    stderr: Vec<u8>,
}

struct TestRun {
    result: TestResult,
    status: Box<dyn status_emitter::TestStatus>,
}

/// A version of `run_tests` that allows more fine-grained control over running tests.
pub fn run_tests_generic(
    mut config: Config,
    args: Args,
    file_filter: impl Fn(&Path, &Args) -> bool + Sync,
    per_file_config: impl Fn(&Config, &Path) -> Option<Config> + Sync,
    status_emitter: impl StatusEmitter + Send,
) -> Result<()> {
    config.fill_host_and_target()?;

    let build_manager = BuildManager::new(&status_emitter);

    let mut results = vec![];

    run_and_collect(
        config.num_test_threads.get(),
        |submit| {
            let mut todo = VecDeque::new();
            todo.push_back(config.root_dir.clone());
            while let Some(path) = todo.pop_front() {
                if path.is_dir() {
                    if path.file_name().unwrap() == "auxiliary" {
                        continue;
                    }
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
                } else if file_filter(&path, &args) {
                    let status = status_emitter.register_test(path);
                    // Forward .rs files to the test workers.
                    submit.send(status).unwrap();
                }
            }
        },
        |receive, finished_files_sender| -> Result<()> {
            for status in receive {
                let path = status.path();
                let maybe_config;
                let config = match per_file_config(&config, path) {
                    None => &config,
                    Some(config) => {
                        maybe_config = config;
                        &maybe_config
                    }
                };
                let result = match std::panic::catch_unwind(|| {
                    parse_and_test_file(&build_manager, &status, config)
                }) {
                    Ok(Ok(res)) => res,
                    Ok(Err(err)) => {
                        finished_files_sender.send(TestRun {
                            result: Err(err),
                            status,
                        })?;
                        continue;
                    }
                    Err(err) => {
                        finished_files_sender.send(TestRun {
                            result: Err(Errored {
                                command: Command::new("<unknown>"),
                                errors: vec![Error::Bug(
                                    *Box::<dyn std::any::Any + Send + 'static>::downcast::<String>(
                                        err,
                                    )
                                    .unwrap(),
                                )],
                                stderr: vec![],
                            }),
                            status,
                        })?;
                        continue;
                    }
                };
                for result in result {
                    finished_files_sender.send(result)?;
                }
            }
            Ok(())
        },
        |finished_files_recv| {
            for run in finished_files_recv {
                run.status.done(&run.result);

                results.push(run);
            }
        },
    )?;

    let mut failures = vec![];
    let mut succeeded = 0;
    let mut ignored = 0;
    let mut filtered = 0;

    for run in results {
        match run.result {
            Ok(TestOk::Ok) => succeeded += 1,
            Ok(TestOk::Ignored) => ignored += 1,
            Ok(TestOk::Filtered) => filtered += 1,
            Err(errored) => failures.push((run.status, errored)),
        }
    }

    let mut failure_emitter = status_emitter.finalize(failures.len(), succeeded, ignored, filtered);
    for (
        status,
        Errored {
            command,
            errors,
            stderr,
        },
    ) in &failures
    {
        let _guard = status.failed_test(command, stderr);
        failure_emitter.test_failure(status, errors);
    }

    if failures.is_empty() {
        Ok(())
    } else {
        Err(eyre!("tests failed"))
    }
}

/// A generic multithreaded runner that has a thread for producing work,
/// a thread for collecting work, and `num_threads` threads for doing the work.
pub fn run_and_collect<SUBMISSION: Send, RESULT: Send>(
    num_threads: usize,
    submitter: impl FnOnce(Sender<SUBMISSION>) + Send,
    runner: impl Sync + Fn(&Receiver<SUBMISSION>, Sender<RESULT>) -> Result<()>,
    collector: impl FnOnce(Receiver<RESULT>) + Send,
) -> Result<()> {
    // A channel for files to process
    let (submit, receive) = unbounded();

    thread::scope(|s| {
        // Create a thread that is in charge of walking the directory and submitting jobs.
        // It closes the channel when it is done.
        s.spawn(|| submitter(submit));

        // A channel for the messages emitted by the individual test threads.
        // Used to produce live updates while running the tests.
        let (finished_files_sender, finished_files_recv) = unbounded();

        s.spawn(|| collector(finished_files_recv));

        let mut threads = vec![];

        // Create N worker threads that receive files to test.
        for _ in 0..num_threads {
            let finished_files_sender = finished_files_sender.clone();
            threads.push(s.spawn(|| runner(&receive, finished_files_sender)));
        }

        for thread in threads {
            thread.join().unwrap()?;
        }
        Ok(())
    })
}

fn parse_and_test_file(
    build_manager: &BuildManager<'_>,
    status: &dyn TestStatus,
    config: &Config,
) -> Result<Vec<TestRun>, Errored> {
    let comments = parse_comments_in_file(status.path())?;
    // Run the test for all revisions
    let revisions = comments
        .revisions
        .clone()
        .unwrap_or_else(|| vec![String::new()]);
    Ok(revisions
        .into_iter()
        .map(|revision| {
            let status = status.for_revision(revision);
            // Ignore file if only/ignore rules do (not) apply
            if !status.test_file_conditions(&comments, config) {
                return TestRun {
                    result: Ok(TestOk::Ignored),
                    status,
                };
            }
            let result = status.run_test(build_manager, config, &comments);
            TestRun { result, status }
        })
        .collect())
}

fn parse_comments_in_file(path: &Path) -> Result<Comments, Errored> {
    match Comments::parse_file(path) {
        Ok(Ok(comments)) => Ok(comments),
        Ok(Err(errors)) => Err(Errored {
            command: Command::new("parse comments"),
            errors,
            stderr: vec![],
        }),
        Err(err) => Err(Errored {
            command: Command::new("parse comments"),
            errors: vec![],
            stderr: format!("{err:?}").into(),
        }),
    }
}

fn build_command(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
) -> Result<Command, Errored> {
    let mut cmd = config.program.build(&config.out_dir);
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
    let edition = comments.edition(revision, config)?;

    if let Some(edition) = edition {
        cmd.arg("--edition").arg(&*edition);
    }

    cmd.envs(
        comments
            .for_revision(revision)
            .flat_map(|r| r.env_vars.iter())
            .map(|(k, v)| (k, v)),
    );

    Ok(cmd)
}

fn build_aux(
    aux_file: &Path,
    config: &Config,
    build_manager: &BuildManager<'_>,
) -> std::result::Result<Vec<OsString>, Errored> {
    let comments = parse_comments_in_file(aux_file)?;
    assert_eq!(
        comments.revisions, None,
        "aux builds cannot specify revisions"
    );

    let mut config = config.clone();

    // Strip any `crate-type` flags from the args, as we need to set our own,
    // and they may conflict (e.g. `lib` vs `proc-macro`);
    let mut prev_was_crate_type = false;
    config.program.args.retain(|arg| {
        if prev_was_crate_type {
            prev_was_crate_type = false;
            return false;
        }
        if arg == "--test" {
            false
        } else if arg == "--crate-type" {
            prev_was_crate_type = true;
            false
        } else if let Some(arg) = arg.to_str() {
            !arg.starts_with("--crate-type=")
        } else {
            true
        }
    });

    let mut config = default_per_file_config(&config, aux_file).unwrap();
    let mut components = aux_file.parent().unwrap().components();

    // Put aux builds into a separate directory per path so that multiple aux files
    // from different directories (but with the same file name) don't collide.
    for c in config.out_dir.components() {
        let deverbatimize = |c| match c {
            Component::Prefix(prefix) => Err(match prefix.kind() {
                Prefix::VerbatimUNC(a, b) => Prefix::UNC(a, b),
                Prefix::VerbatimDisk(d) => Prefix::Disk(d),
                other => other,
            }),
            c => Ok(c),
        };
        let c2 = components.next();
        if Some(deverbatimize(c)) != c2.map(deverbatimize) {
            config.out_dir.extend(c2);
            config.out_dir.extend(components);
            break;
        }
    }

    let mut aux_cmd = build_command(aux_file, &config, "", &comments)?;

    let mut extra_args = build_aux_files(
        aux_file.parent().unwrap(),
        &comments,
        "",
        &config,
        build_manager,
    )?;
    // Make sure we see our dependencies
    aux_cmd.args(extra_args.iter());

    aux_cmd.arg("--emit=link");
    let filename = aux_file.file_stem().unwrap().to_str().unwrap();
    let output = aux_cmd.output().unwrap();
    if !output.status.success() {
        let error = Error::Command {
            kind: "compilation of aux build failed".to_string(),
            status: output.status,
        };
        return Err(Errored {
            command: aux_cmd,
            errors: vec![error],
            stderr: rustc_stderr::process(aux_file, &output.stderr).rendered,
        });
    }

    // Now run the command again to fetch the output filenames
    aux_cmd.arg("--print").arg("file-names");
    let output = aux_cmd.output().unwrap();
    assert!(output.status.success());

    for file in output.stdout.lines() {
        let file = std::str::from_utf8(file).unwrap();
        let crate_name = filename.replace('-', "_");
        let path = config.out_dir.join(file);
        extra_args.push("--extern".into());
        let mut cname = OsString::from(&crate_name);
        cname.push("=");
        cname.push(path);
        extra_args.push(cname);
        // Help cargo find the crates added with `--extern`.
        extra_args.push("-L".into());
        extra_args.push(config.out_dir.as_os_str().to_os_string());
    }
    Ok(extra_args)
}

impl dyn TestStatus {
    fn run_test(
        &self,
        build_manager: &BuildManager<'_>,
        config: &Config,
        comments: &Comments,
    ) -> TestResult {
        let extra_args = build_manager.build(Build::Dependencies, config)?;
        let mut config = config.clone();
        config.program.args.extend(extra_args);
        let config = &config;

        let path = self.path();
        let revision = self.revision();

        let extra_args = build_aux_files(
            &path.parent().unwrap().join("auxiliary"),
            comments,
            revision,
            config,
            build_manager,
        )?;

        let mut cmd = build_command(path, config, revision, comments)?;
        cmd.args(&extra_args);

        let (status, stderr, stdout) = self.run_command(&mut cmd);

        let mode = config.mode.maybe_override(comments, revision)?;

        match *mode {
            Mode::Run { .. } if Mode::Pass.ok(status).is_ok() => {
                return run_test_binary(mode, path, revision, comments, cmd, config)
            }
            Mode::Panic | Mode::Yolo { .. } => {}
            Mode::Run { .. } | Mode::Pass | Mode::Fail { .. } => {
                if status.code() == Some(101) {
                    let stderr = String::from_utf8_lossy(&stderr);
                    let stdout = String::from_utf8_lossy(&stdout);
                    return Err(Errored {
                        command: cmd,
                        errors: vec![Error::Bug(format!(
                            "test panicked: stderr:\n{stderr}\nstdout:\n{stdout}",
                        ))],
                        stderr: vec![],
                    });
                }
            }
        }
        check_test_result(
            cmd, *mode, path, config, revision, comments, status, stdout, &stderr,
        )?;
        run_rustfix(&stderr, path, comments, revision, config, *mode, extra_args)?;
        Ok(TestOk::Ok)
    }

    /// Run a command, and if it takes more than 100ms, start appending the last stderr/stdout
    /// line to the current status spinner.
    fn run_command(&self, cmd: &mut Command) -> (ExitStatus, Vec<u8>, Vec<u8>) {
        let mut child = cmd
            .stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .stdin(Stdio::null())
            .spawn()
            .unwrap_or_else(|err| panic!("could not execute {cmd:?}: {err}"));

        let stdout = ReadHelper::from(child.stdout.take().unwrap());
        let mut stderr = ReadHelper::from(child.stderr.take().unwrap());

        let start = std::time::Instant::now();
        let status = loop {
            if let Some(exit) = child.try_wait().unwrap() {
                break exit;
            }
            if start.elapsed() < Duration::from_millis(1000) {
                std::thread::sleep(Duration::from_millis(5));
                continue;
            }

            let status = stderr.last_line();
            if !status.is_empty() {
                self.update_status(status.to_str_lossy().to_string())
            }
            std::thread::sleep(Duration::from_millis(100));
        };
        (status, stderr.read_to_end(), stdout.read_to_end())
    }
}

fn build_aux_files(
    aux_dir: &Path,
    comments: &Comments,
    revision: &str,
    config: &Config,
    build_manager: &BuildManager<'_>,
) -> Result<Vec<OsString>, Errored> {
    let mut extra_args = vec![];
    for rev in comments.for_revision(revision) {
        for aux in &rev.aux_builds {
            let line = aux.line();
            let aux = &**aux;
            let aux_file = if aux.starts_with("..") {
                aux_dir.parent().unwrap().join(aux)
            } else {
                aux_dir.join(aux)
            };
            extra_args.extend(
                build_manager
                    .build(
                        Build::Aux {
                            aux_file: aux_file.canonicalize().unwrap(),
                        },
                        config,
                    )
                    .map_err(
                        |Errored {
                             command,
                             errors,
                             stderr,
                         }| Errored {
                            command,
                            errors: vec![Error::Aux {
                                path: aux_file,
                                errors,
                                line,
                            }],
                            stderr,
                        },
                    )?,
            );
        }
    }
    Ok(extra_args)
}

fn run_test_binary(
    mode: MaybeWithLine<Mode>,
    path: &Path,
    revision: &str,
    comments: &Comments,
    mut cmd: Command,
    config: &Config,
) -> TestResult {
    cmd.arg("--print").arg("file-names");
    let output = cmd.output().unwrap();
    assert!(output.status.success());

    let mut files = output.stdout.lines();
    let file = files.next().unwrap();
    assert_eq!(files.next(), None);
    let file = std::str::from_utf8(file).unwrap();
    let exe = config.out_dir.join(file);
    let mut exe = Command::new(exe);
    let output = exe.output().unwrap();

    let mut errors = vec![];

    check_test_output(
        path,
        &mut errors,
        revision,
        config,
        comments,
        &output.stdout,
        &output.stderr,
    );

    errors.extend(mode.ok(output.status).err());
    if errors.is_empty() {
        Ok(TestOk::Ok)
    } else {
        Err(Errored {
            command: exe,
            errors,
            stderr: vec![],
        })
    }
}

fn run_rustfix(
    stderr: &[u8],
    path: &Path,
    comments: &Comments,
    revision: &str,
    config: &Config,
    mode: Mode,
    extra_args: Vec<OsString>,
) -> Result<(), Errored> {
    let no_run_rustfix =
        comments.find_one_for_revision(revision, "`no-rustfix` annotations", |r| r.no_rustfix)?;

    let global_rustfix = match mode {
        Mode::Pass | Mode::Run { .. } | Mode::Panic => false,
        Mode::Fail { rustfix, .. } | Mode::Yolo { rustfix } => rustfix,
    };

    let fixed_code = (no_run_rustfix.is_none() && global_rustfix)
        .then_some(())
        .and_then(|()| {
            let suggestions = std::str::from_utf8(stderr)
                .unwrap()
                .lines()
                .flat_map(|line| {
                    if !line.starts_with('{') {
                        return vec![];
                    }
                    rustfix::get_suggestions_from_json(
                        line,
                        &HashSet::new(),
                        if let Mode::Yolo { .. } = config.mode {
                            rustfix::Filter::Everything
                        } else {
                            rustfix::Filter::MachineApplicableOnly
                        },
                    )
                    .unwrap_or_else(|err| {
                        panic!("could not deserialize diagnostics json for rustfix {err}:{line}")
                    })
                })
                .collect::<Vec<_>>();
            if suggestions.is_empty() {
                None
            } else {
                Some(rustfix::apply_suggestions(
                    &std::fs::read_to_string(path).unwrap(),
                    &suggestions,
                ))
            }
        })
        .transpose()
        .map_err(|err| Errored {
            command: Command::new(format!("rustfix {}", path.display())),
            errors: vec![Error::Rustfix(err)],
            stderr: stderr.into(),
        })?;

    let edition = comments.edition(revision, config)?;
    let edition = edition
        .map(|mwl| {
            let line = mwl.line().unwrap_or(NonZeroUsize::MAX);
            WithLine::new(mwl.into_inner(), line)
        })
        .into();
    let rustfix_comments = Comments {
        revisions: None,
        revisioned: std::iter::once((
            vec![],
            Revisioned {
                line: NonZeroUsize::MAX,
                ignore: vec![],
                only: vec![],
                stderr_per_bitwidth: false,
                compile_flags: comments
                    .for_revision(revision)
                    .flat_map(|r| r.compile_flags.iter().cloned())
                    .collect(),
                env_vars: comments
                    .for_revision(revision)
                    .flat_map(|r| r.env_vars.iter().cloned())
                    .collect(),
                normalize_stderr: vec![],
                error_in_other_files: vec![],
                error_matches: vec![],
                require_annotations_for_level: Default::default(),
                aux_builds: comments
                    .for_revision(revision)
                    .flat_map(|r| r.aux_builds.iter().cloned())
                    .collect(),
                edition,
                mode: OptWithLine::new(Mode::Pass, NonZeroUsize::MAX),
                no_rustfix: OptWithLine::new((), NonZeroUsize::MAX),
                needs_asm_support: false,
            },
        ))
        .collect(),
    };

    let run = fixed_code.is_some();
    let mut errors = vec![];
    let rustfix_path = check_output(
        // Always check for `.fixed` files, even if there were reasons not to run rustfix.
        // We don't want to leave around stray `.fixed` files
        fixed_code.unwrap_or_default().as_bytes(),
        path,
        &mut errors,
        revised(revision, "fixed"),
        &Filter::default(),
        config,
        &rustfix_comments,
        revision,
    );
    if !errors.is_empty() {
        return Err(Errored {
            command: Command::new(format!("checking {}", path.display())),
            errors,
            stderr: vec![],
        });
    }

    if !run {
        return Ok(());
    }

    let mut cmd = build_command(&rustfix_path, config, revision, &rustfix_comments)?;
    cmd.args(extra_args);
    // picking the crate name from the file name is problematic when `.revision_name` is inserted
    cmd.arg("--crate-name").arg(
        path.file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('-', "_"),
    );
    let output = cmd.output().unwrap();
    if output.status.success() {
        Ok(())
    } else {
        Err(Errored {
            command: cmd,
            errors: vec![Error::Command {
                kind: "rustfix".into(),
                status: output.status,
            }],
            stderr: rustc_stderr::process(&rustfix_path, &output.stderr).rendered,
        })
    }
}

fn revised(revision: &str, extension: &str) -> String {
    if revision.is_empty() {
        extension.to_string()
    } else {
        format!("{revision}.{extension}")
    }
}

fn check_test_result(
    command: Command,
    mode: Mode,
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
    status: ExitStatus,
    stdout: Vec<u8>,
    stderr: &[u8],
) -> Result<(), Errored> {
    let mut errors = vec![];
    errors.extend(mode.ok(status).err());
    // Always remove annotation comments from stderr.
    let diagnostics = rustc_stderr::process(path, stderr);
    check_test_output(
        path,
        &mut errors,
        revision,
        config,
        comments,
        &stdout,
        &diagnostics.rendered,
    );
    // Check error annotations in the source against output
    check_annotations(
        diagnostics.messages,
        diagnostics.messages_from_unknown_file_or_line,
        path,
        &mut errors,
        config,
        revision,
        comments,
    )?;
    if errors.is_empty() {
        Ok(())
    } else {
        Err(Errored {
            command,
            errors,
            stderr: diagnostics.rendered,
        })
    }
}

fn check_test_output(
    path: &Path,
    errors: &mut Vec<Error>,
    revision: &str,
    config: &Config,
    comments: &Comments,
    stdout: &[u8],
    stderr: &[u8],
) {
    // Check output files (if any)
    // Check output files against actual output
    check_output(
        stderr,
        path,
        errors,
        revised(revision, "stderr"),
        &config.stderr_filters,
        config,
        comments,
        revision,
    );
    check_output(
        stdout,
        path,
        errors,
        revised(revision, "stdout"),
        &config.stdout_filters,
        config,
        comments,
        revision,
    );
}

fn check_annotations(
    mut messages: Vec<Vec<Message>>,
    mut messages_from_unknown_file_or_line: Vec<Message>,
    path: &Path,
    errors: &mut Errors,
    config: &Config,
    revision: &str,
    comments: &Comments,
) -> Result<(), Errored> {
    let error_patterns = comments
        .for_revision(revision)
        .flat_map(|r| r.error_in_other_files.iter());

    let mut seen_error_match = false;
    for error_pattern in error_patterns {
        seen_error_match = true;
        // first check the diagnostics messages outside of our file. We check this first, so that
        // you can mix in-file annotations with //@error-in-other-file annotations, even if there is overlap
        // in the messages.
        if let Some(i) = messages_from_unknown_file_or_line
            .iter()
            .position(|msg| error_pattern.matches(&msg.message))
        {
            messages_from_unknown_file_or_line.remove(i);
        } else {
            errors.push(Error::PatternNotFound(error_pattern.clone()));
        }
    }

    // The order on `Level` is such that `Error` is the highest level.
    // We will ensure that *all* diagnostics of level at least `lowest_annotation_level`
    // are matched.
    let mut lowest_annotation_level = MaybeWithLine::new_config(Level::Error);
    for &ErrorMatch {
        ref pattern,
        level,
        line,
    } in comments
        .for_revision(revision)
        .flat_map(|r| r.error_matches.iter())
    {
        seen_error_match = true;
        // If we found a diagnostic with a level annotation, make sure that all
        // diagnostics of that level have annotations, even if we don't end up finding a matching diagnostic
        // for this pattern.
        if *lowest_annotation_level > level {
            lowest_annotation_level = MaybeWithLine::new(level, line);
        }

        if let Some(msgs) = messages.get_mut(line.get()) {
            let found = msgs
                .iter()
                .position(|msg| pattern.matches(&msg.message) && msg.level == level);
            if let Some(found) = found {
                msgs.remove(found);
                continue;
            }
        }

        errors.push(Error::PatternNotFound(pattern.clone()));
    }

    let required_annotation_level = comments.find_one_for_revision(
        revision,
        "`require_annotations_for_level` annotations",
        |r| r.require_annotations_for_level,
    )?;

    let required_annotation_level =
        required_annotation_level.map_or(*lowest_annotation_level, |l| *l);
    let filter = |mut msgs: Vec<Message>| -> Vec<_> {
        msgs.retain(|msg| msg.level >= required_annotation_level);
        msgs
    };

    let mode = config.mode.maybe_override(comments, revision)?;

    if !matches!(config.mode, Mode::Yolo { .. }) {
        let messages_from_unknown_file_or_line = filter(messages_from_unknown_file_or_line);
        if !messages_from_unknown_file_or_line.is_empty() {
            errors.push(Error::ErrorsWithoutPattern {
                path: None,
                msgs: messages_from_unknown_file_or_line,
            });
        }

        for (line, msgs) in messages.into_iter().enumerate() {
            let msgs = filter(msgs);
            if !msgs.is_empty() {
                let line = NonZeroUsize::new(line).expect("line 0 is always empty");
                errors.push(Error::ErrorsWithoutPattern {
                    path: Some(WithLine::new(path.to_path_buf(), line)),
                    msgs,
                });
            }
        }
    }

    match (*mode, seen_error_match) {
        (Mode::Pass, true) | (Mode::Panic, true) => errors.push(Error::PatternFoundInPassTest),
        (
            Mode::Fail {
                require_patterns: true,
                ..
            },
            false,
        ) => errors.push(Error::NoPatternsFound),
        _ => {}
    }
    Ok(())
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
) -> PathBuf {
    let target = config.target.as_ref().unwrap();
    let output = normalize(path, output, filters, comments, revision);
    let path = output_path(path, comments, kind, target, revision);
    match &config.output_conflict_handling {
        OutputConflictHandling::Bless => {
            if output.is_empty() {
                let _ = std::fs::remove_file(&path);
            } else {
                std::fs::write(&path, &output).unwrap();
            }
        }
        OutputConflictHandling::Error(bless_command) => {
            let expected_output = std::fs::read(&path).unwrap_or_default();
            if output != expected_output {
                errors.push(Error::OutputDiffers {
                    path: path.clone(),
                    actual: output,
                    expected: expected_output,
                    bless_command: bless_command.clone(),
                });
            }
        }
        OutputConflictHandling::Ignore => {}
    }
    path
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
        Condition::Host(t) => config.host.as_ref().unwrap().contains(t),
        Condition::OnHost => target == config.host.as_ref().unwrap(),
    }
}

impl dyn TestStatus {
    /// Returns whether according to the in-file conditions, this file should be run.
    fn test_file_conditions(&self, comments: &Comments, config: &Config) -> bool {
        let revision = self.revision();
        if comments
            .for_revision(revision)
            .flat_map(|r| r.ignore.iter())
            .any(|c| test_condition(c, config))
        {
            return false;
        }
        if comments
            .for_revision(revision)
            .any(|r| r.needs_asm_support && !config.has_asm_support())
        {
            return false;
        }
        comments
            .for_revision(revision)
            .flat_map(|r| r.only.iter())
            .all(|c| test_condition(c, config))
    }
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
    let path_filter = (Match::from(path.parent().unwrap()), b"$DIR" as &[u8]);
    let filters = filters.iter().chain(std::iter::once(&path_filter));
    let mut text = text.to_owned();
    if let Some(lib_path) = option_env!("RUSTC_LIB_PATH") {
        text = text.replace(lib_path, "RUSTLIB");
    }

    for (rule, replacement) in filters {
        text = rule.replace_all(&text, replacement).into_owned();
    }

    for (from, to) in comments
        .for_revision(revision)
        .flat_map(|r| r.normalize_stderr.iter())
    {
        text = from.replace_all(&text, to).into_owned();
    }
    text
}
