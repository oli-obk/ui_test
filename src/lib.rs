#![allow(
    clippy::enum_variant_names,
    clippy::useless_format,
    clippy::too_many_arguments,
    rustc::internal
)]
#![deny(missing_docs)]

//! A crate to run the Rust compiler (or other binaries) and test their command line output.

use bstr::ByteSlice;
use clap::Parser;
pub use color_eyre;
use color_eyre::eyre::{eyre, Result};
use crossbeam_channel::{unbounded, Receiver, Sender};
use lazy_static::lazy_static;
use parser::{ErrorMatch, OptWithLine, Revisioned, WithLine};
use regex::bytes::{Captures, Regex};
use rustc_stderr::{Diagnostics, Level, Message};
use status_emitter::StatusEmitter;
use std::borrow::Cow;
use std::collections::{HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread;

use crate::parser::{Comments, Condition};

mod cmd;
mod config;
mod dependencies;
mod diff;
mod error;
pub mod github_actions;
mod mode;
mod parser;
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
                        r#"(?x)
                        (?:
                            # Match paths to files with extensions that don't include spaces
                            \\(?:[\pL\pN.\-_']+[/\\])*[\pL\pN.\-_']+\.\pL+
                        |
                            # Allow spaces in absolute paths
                            [A-Z]:\\(?:[\pL\pN.\-_'\ ]+[/\\])+
                        )"#,
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
        if v.starts_with(r#"\\?\"#) {
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
    eprintln!("   Compiler: {}", config.program.display());

    let name = config.root_dir.display().to_string();

    let args = Args::parse();

    run_tests_generic(
        config,
        args,
        default_file_filter,
        default_per_file_config,
        (
            status_emitter::Text::verbose(),
            status_emitter::Gha::<true> { name },
        ),
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
    if file_contents.find(b"#[proc_macro]").is_some()
        || file_contents.find(b"#[proc_macro_attribute]").is_some()
        || file_contents.find(b"#[proc_macro_derive]").is_some()
    {
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
    config.build_dependencies_and_link_them()?;

    let comments =
        Comments::parse_file(path)?.map_err(|errors| color_eyre::eyre::eyre!("{errors:#?}"))?;
    let mut errors = vec![];
    let result = build_command(path, &config, "", &comments, &mut errors);
    assert!(errors.is_empty(), "{errors:#?}");
    Ok(result)
}

#[allow(clippy::large_enum_variant)]
/// The possible results a single test can have.
pub enum TestResult {
    /// The test passed
    Ok,
    /// The test was ignored due to a rule (`//@only-*` or `//@ignore-*`)
    Ignored,
    /// The test was filtered with the `file_filter` argument.
    Filtered,
    /// The test failed.
    Errored {
        /// Command that failed
        command: Command,
        /// The errors that were encountered.
        errors: Vec<Error>,
        /// The full stderr of the test run.
        stderr: Vec<u8>,
    },
}

struct TestRun {
    result: TestResult,
    path: PathBuf,
    revision: String,
}

/// A version of `run_tests` that allows more fine-grained control over running tests.
pub fn run_tests_generic(
    mut config: Config,
    args: Args,
    file_filter: impl Fn(&Path, &Args) -> bool + Sync,
    per_file_config: impl Fn(&Config, &Path) -> Option<Config> + Sync,
    mut status_emitter: impl StatusEmitter + Send,
) -> Result<()> {
    config.fill_host_and_target()?;

    config.build_dependencies_and_link_them()?;

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
                    // Forward .rs files to the test workers.
                    submit.send(path).unwrap();
                }
            }
        },
        |receive, finished_files_sender| -> Result<()> {
            for path in receive {
                let maybe_config;
                let config = match per_file_config(&config, &path) {
                    None => &config,
                    Some(config) => {
                        maybe_config = config;
                        &maybe_config
                    }
                };
                let result = match std::panic::catch_unwind(|| parse_and_test_file(&path, config)) {
                    Ok(res) => res,
                    Err(err) => {
                        finished_files_sender.send(TestRun {
                            result: TestResult::Errored {
                                command: Command::new("<unknown>"),
                                errors: vec![Error::Bug(
                                    *Box::<dyn std::any::Any + Send + 'static>::downcast::<String>(
                                        err,
                                    )
                                    .unwrap(),
                                )],
                                stderr: vec![],
                            },
                            path,
                            revision: String::new(),
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
                status_emitter.test_result(&run.path, &run.revision, &run.result);

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

    let mut failure_emitter = status_emitter.finalize(failures.len(), succeeded, ignored, filtered);
    for (path, command, revision, errors, stderr) in &failures {
        let _guard = status_emitter.failed_test(revision, path, command, stderr);
        failure_emitter.test_failure(path, revision, errors);
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

fn parse_and_test_file(path: &Path, config: &Config) -> Vec<TestRun> {
    let comments = match parse_comments_in_file(path) {
        Ok(comments) => comments,
        Err((stderr, errors)) => {
            return vec![TestRun {
                result: TestResult::Errored {
                    command: Command::new("parse comments"),
                    errors,
                    stderr,
                },
                path: path.into(),
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
                    path: path.into(),
                    revision,
                };
            }
            let (command, errors, stderr) = run_test(path, config, &revision, &comments);
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
                path: path.into(),
            }
        })
        .collect()
}

fn parse_comments_in_file(path: &Path) -> Result<Comments, (Vec<u8>, Vec<Error>)> {
    match Comments::parse_file(path) {
        Ok(Ok(comments)) => Ok(comments),
        Ok(Err(errors)) => Err((vec![], errors)),
        Err(err) => Err((format!("{err:?}").into(), vec![])),
    }
}

fn build_command(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
    errors: &mut Vec<Error>,
) -> Command {
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
    comments.edition(revision, config).map(|(edition, error)| {
        cmd.arg("--edition").arg(edition);
        errors.extend(error);
    });

    cmd.envs(
        comments
            .for_revision(revision)
            .flat_map(|r| r.env_vars.iter())
            .map(|(k, v)| (k, v)),
    );

    cmd
}

fn build_aux(
    aux_file: &Path,
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
    kind: &str,
    aux: &Path,
    extra_args: &mut Vec<String>,
) -> std::result::Result<(), (Command, Vec<Error>, Vec<u8>)> {
    let comments = match parse_comments_in_file(aux_file) {
        Ok(comments) => comments,
        Err((msg, mut errors)) => {
            return Err((
                build_command(path, config, revision, comments, &mut errors),
                errors,
                msg,
            ))
        }
    };
    assert_eq!(comments.revisions, None);

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

    // Put aux builds into a separate directory per test so that
    // tests running in parallel but building the same aux build don't conflict.
    // FIXME: put aux builds into the regular build queue.
    config.out_dir = config.out_dir.join(path.with_extension(""));

    let mut errors = vec![];

    let mut aux_cmd = build_command(aux_file, &config, revision, &comments, &mut errors);

    if !errors.is_empty() {
        return Err((aux_cmd, errors, vec![]));
    }

    let current_extra_args =
        build_aux_files(aux_file, aux_file.parent().unwrap(), &comments, "", &config)?;
    // Make sure we see our dependencies
    aux_cmd.args(current_extra_args.iter());
    // Make sure our dependents also see our dependencies.
    extra_args.extend(current_extra_args);

    aux_cmd.arg("--crate-type").arg(kind);
    aux_cmd.arg("--emit=link");
    let filename = aux.file_stem().unwrap().to_str().unwrap();
    let output = aux_cmd.output().unwrap();
    if !output.status.success() {
        let error = Error::Command {
            kind: "compilation of aux build failed".to_string(),
            status: output.status,
        };
        return Err((
            aux_cmd,
            vec![error],
            rustc_stderr::process(path, &output.stderr).rendered,
        ));
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
        extra_args.push(format!("{crate_name}={}", path.display()));
        // Help cargo find the crates added with `--extern`.
        extra_args.push("-L".into());
        extra_args.push(config.out_dir.display().to_string());
    }
    Ok(())
}

fn run_test(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
) -> (Command, Errors, Vec<u8>) {
    let extra_args = match build_aux_files(
        path,
        &path.parent().unwrap().join("auxiliary"),
        comments,
        revision,
        config,
    ) {
        Ok(value) => value,
        Err(value) => return value,
    };

    let mut errors = vec![];

    let mut cmd = build_command(path, config, revision, comments, &mut errors);
    cmd.args(&extra_args);

    let output = cmd
        .output()
        .unwrap_or_else(|err| panic!("could not execute {cmd:?}: {err}"));
    let mode = config
        .mode
        .maybe_override(comments, revision)
        .map(|(mode, error)| {
            errors.extend(error);
            mode
        });
    let status_check = mode.ok(output.status);
    if matches!(*mode, Mode::Run { .. }) && Mode::Pass.ok(output.status).is_empty() {
        let cmd = run_test_binary(mode, path, revision, comments, cmd, config, &mut errors);
        return (cmd, errors, vec![]);
    }
    errors.extend(status_check);
    if output.status.code() == Some(101) && !matches!(config.mode, Mode::Panic | Mode::Yolo) {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        errors.push(Error::Bug(format!(
            "test panicked: stderr:\n{stderr}\nstdout:\n{stdout}",
        )));
        return (cmd, errors, vec![]);
    }
    // Always remove annotation comments from stderr.
    let diagnostics = rustc_stderr::process(path, &output.stderr);
    let rustfixed = run_rustfix(
        &output.stderr,
        path,
        comments,
        revision,
        config,
        extra_args,
        &mut errors,
    );
    let stderr = check_test_result(
        path,
        config,
        revision,
        comments,
        &mut errors,
        &output.stdout,
        diagnostics,
    );
    if let Some((mut rustfix, rustfix_path)) = rustfixed {
        // picking the crate name from the file name is problematic when `.revision_name` is inserted
        rustfix.arg("--crate-name").arg(
            path.file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .replace('-', "_"),
        );
        let output = rustfix.output().unwrap();
        if !output.status.success() {
            errors.push(Error::Command {
                kind: "rustfix".into(),
                status: output.status,
            });
            return (
                rustfix,
                errors,
                rustc_stderr::process(&rustfix_path, &output.stderr).rendered,
            );
        }
    }
    (cmd, errors, stderr)
}

fn build_aux_files(
    path: &Path,
    aux_dir: &Path,
    comments: &Comments,
    revision: &str,
    config: &Config,
) -> Result<Vec<String>, (Command, Vec<Error>, Vec<u8>)> {
    let mut extra_args = vec![];
    for rev in comments.for_revision(revision) {
        for aux in &rev.aux_builds {
            let line = aux.line();
            let (aux, kind) = &**aux;
            let aux_file = if aux.starts_with("..") {
                aux_dir.parent().unwrap().join(aux)
            } else {
                aux_dir.join(aux)
            };
            if let Err((command, errors, msg)) = build_aux(
                &aux_file,
                path,
                config,
                revision,
                comments,
                kind,
                aux,
                &mut extra_args,
            ) {
                return Err((
                    command,
                    vec![Error::Aux {
                        path: aux_file,
                        errors,
                        line,
                    }],
                    msg,
                ));
            }
        }
    }
    Ok(extra_args)
}

fn run_test_binary(
    mode: WithLine<Mode>,
    path: &Path,
    revision: &str,
    comments: &Comments,
    mut cmd: Command,
    config: &Config,
    errors: &mut Vec<Error>,
) -> Command {
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

    check_test_output(
        path,
        errors,
        revision,
        config,
        comments,
        &output.stdout,
        &output.stderr,
    );

    errors.extend(mode.ok(output.status));

    exe
}

fn run_rustfix(
    stderr: &[u8],
    path: &Path,
    comments: &Comments,
    revision: &str,
    config: &Config,
    extra_args: Vec<String>,
    errors: &mut Vec<Error>,
) -> Option<(Command, PathBuf)> {
    let no_run_rustfix = comments
        .find_one_for_revision(revision, "`no-rustfix` annotations", |r| r.no_rustfix)
        .map(|(wl, error)| {
            errors.extend(error);
            wl
        });
    let fixed_code = (no_run_rustfix.is_none() && config.rustfix)
        .then(|| ())
        .and_then(|()| {
            let input = std::str::from_utf8(stderr).unwrap();
            let suggestions = rustfix::get_suggestions_from_json(
                input,
                &HashSet::new(),
                if let Mode::Yolo = config.mode {
                    rustfix::Filter::Everything
                } else {
                    rustfix::Filter::MachineApplicableOnly
                },
            )
            .unwrap_or_else(|err| {
                panic!("could not deserialize diagnostics json for rustfix {err}:{input}")
            });
            if suggestions.is_empty() {
                return None;
            }

            let fixed_code = match rustfix::apply_suggestions(
                &std::fs::read_to_string(path).unwrap(),
                &suggestions,
            ) {
                Ok(fixed_code) => fixed_code,
                Err(e) => {
                    errors.push(Error::Rustfix(e));
                    return None;
                }
            };
            Some(fixed_code)
        });

    let edition = comments.edition(revision, config).map(|(edition, error)| {
        errors.extend(error);
        edition
    });
    let rustfix_comments = Comments {
        revisions: None,
        revisioned: std::iter::once((
            vec![],
            Revisioned {
                line: 0,
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
                edition: edition.into(),
                mode: OptWithLine::new(Mode::Pass, 0),
                no_rustfix: OptWithLine::new((), 0),
                needs_asm_support: false,
            },
        ))
        .collect(),
    };

    let run = fixed_code.is_some();
    let path = check_output(
        // Always check for `.fixed` files, even if there were reasons not to run rustfix.
        // We don't want to leave around stray `.fixed` files
        fixed_code.unwrap_or_default().as_bytes(),
        path,
        errors,
        revised(revision, "fixed"),
        &Filter::default(),
        config,
        &rustfix_comments,
        revision,
    );

    run.then(|| {
        let mut cmd = build_command(&path, config, revision, &rustfix_comments, errors);
        cmd.args(extra_args);
        (cmd, path)
    })
}

fn revised(revision: &str, extension: &str) -> String {
    if revision.is_empty() {
        extension.to_string()
    } else {
        format!("{revision}.{extension}")
    }
}

fn check_test_result(
    path: &Path,
    config: &Config,
    revision: &str,
    comments: &Comments,
    errors: &mut Errors,
    stdout: &[u8],
    diagnostics: Diagnostics,
) -> Vec<u8> {
    check_test_output(
        path,
        errors,
        revision,
        config,
        comments,
        stdout,
        &diagnostics.rendered,
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
) {
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
    let mut lowest_annotation_level = WithLine::new(Level::Error, 0);
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
            lowest_annotation_level = WithLine::new(level, line);
        }

        if let Some(msgs) = messages.get_mut(line) {
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

    let required_annotation_level = comments
        .find_one_for_revision(
            revision,
            "`require_annotations_for_level` annotations",
            |r| r.require_annotations_for_level,
        )
        .map(|(lvl, error)| {
            errors.extend(error);
            lvl
        })
        .unwrap_or(lowest_annotation_level);
    let filter = |mut msgs: Vec<Message>| -> Vec<_> {
        msgs.retain(|msg| msg.level >= *required_annotation_level);
        msgs
    };

    let mode = config
        .mode
        .maybe_override(comments, revision)
        .map(|(mode, error)| {
            errors.extend(error);
            mode
        });

    if !matches!(config.mode, Mode::Yolo) {
        let messages_from_unknown_file_or_line = filter(messages_from_unknown_file_or_line);
        if !messages_from_unknown_file_or_line.is_empty() {
            errors.push(Error::ErrorsWithoutPattern {
                path: Default::default(),
                msgs: messages_from_unknown_file_or_line,
            });
        }

        for (line, msgs) in messages.into_iter().enumerate() {
            let msgs = filter(msgs);
            if !msgs.is_empty() {
                errors.push(Error::ErrorsWithoutPattern {
                    path: OptWithLine::new(path.to_path_buf(), line),
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

/// Returns whether according to the in-file conditions, this file should be run.
fn test_file_conditions(comments: &Comments, config: &Config, revision: &str) -> bool {
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
