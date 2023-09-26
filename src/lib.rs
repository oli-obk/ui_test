#![allow(
    clippy::enum_variant_names,
    clippy::useless_format,
    clippy::too_many_arguments,
    rustc::internal
)]
#![deny(missing_docs)]

//! A crate to run the Rust compiler (or other binaries) and test their command line output.

use bstr::ByteSlice;
pub use color_eyre;
use color_eyre::eyre::{eyre, Result};
use crossbeam_channel::{unbounded, Receiver, Sender};
use dependencies::{Build, BuildManager};
use lazy_static::lazy_static;
use parser::{ErrorMatch, MaybeSpanned, OptWithLine, Revisioned, Spanned};
use regex::bytes::{Captures, Regex};
use rustc_stderr::{Level, Message, Span};
use status_emitter::{StatusEmitter, TestStatus};
use std::borrow::Cow;
use std::collections::{HashSet, VecDeque};
use std::ffi::OsString;
use std::num::NonZeroUsize;
use std::path::{Component, Path, PathBuf, Prefix};
use std::process::{Command, ExitStatus, Output};
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
pub fn run_tests(mut config: Config) -> Result<()> {
    let args = Args::test()?;
    if let Format::Pretty = args.format {
        println!("Compiler: {}", config.program.display());
    }

    let name = config.root_dir.display().to_string();

    let text = match args.format {
        Format::Terse => status_emitter::Text::quiet(),
        Format::Pretty => status_emitter::Text::verbose(),
    };
    config.with_args(&args, true);

    run_tests_generic(
        vec![config],
        default_file_filter,
        default_per_file_config,
        (text, status_emitter::Gha::<true> { name }),
    )
}

/// The filter used by `run_tests` to only run on `.rs` files that are
/// specified by [`Config::filter_files`] and [`Config::skip_files`].
pub fn default_file_filter(path: &Path, config: &Config) -> bool {
    path.extension().is_some_and(|ext| ext == "rs") && default_any_file_filter(path, config)
}

/// Run on all files that are specified by [`Config::filter_files`] and
/// [`Config::skip_files`].
///
/// To only include rust files see [`default_file_filter`].
pub fn default_any_file_filter(path: &Path, config: &Config) -> bool {
    let path = path.display().to_string();
    let contains_path = |files: &[String]| {
        files.iter().any(|f| {
            if config.filter_exact {
                *f == path
            } else {
                path.contains(f)
            }
        })
    };

    if contains_path(&config.skip_files) {
        return false;
    }

    config.filter_files.is_empty() || contains_path(&config.filter_files)
}

/// The default per-file config used by `run_tests`.
pub fn default_per_file_config(config: &mut Config, _path: &Path, file_contents: &[u8]) {
    // Heuristic:
    // * if the file contains `#[test]`, automatically pass `--cfg test`.
    // * if the file does not contain `fn main()` or `#[start]`, automatically pass `--crate-type=lib`.
    // This avoids having to spam `fn main() {}` in almost every test.
    if file_contents.find(b"#[proc_macro").is_some() {
        config.program.args.push("--crate-type=proc-macro".into())
    } else if file_contents.find(b"#[test]").is_some() {
        config.program.args.push("--test".into());
    } else if file_contents.find(b"fn main()").is_none()
        && file_contents.find(b"#[start]").is_none()
    {
        config.program.args.push("--crate-type=lib".into());
    }
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
    /// The full stdout of the test run.
    stdout: Vec<u8>,
}

struct TestRun {
    result: TestResult,
    status: Box<dyn status_emitter::TestStatus>,
}

/// A version of `run_tests` that allows more fine-grained control over running tests.
///
/// All `configs` are being run in parallel.
/// If multiple configs are provided, the [`Config::threads`] value of the first one is used;
/// the thread count of all other configs is ignored.
pub fn run_tests_generic(
    mut configs: Vec<Config>,
    file_filter: impl Fn(&Path, &Config) -> bool + Sync,
    per_file_config: impl Fn(&mut Config, &Path, &[u8]) + Sync,
    status_emitter: impl StatusEmitter + Send,
) -> Result<()> {
    // Nexttest emulation: we act as if we are one single test.
    if configs.iter().any(|c| c.list) {
        if configs.iter().any(|c| !c.run_only_ignored) {
            println!("ui_test: test");
        }
        return Ok(());
    }
    for config in &mut configs {
        if config.filter_exact
            && config.filter_files.len() == 1
            && config.filter_files[0] == "ui_test"
        {
            config.filter_exact = false;
            config.filter_files.clear();
        }
    }

    for config in &mut configs {
        config.fill_host_and_target()?;
    }

    let build_manager = BuildManager::new(&status_emitter);

    let mut results = vec![];

    let num_threads = match configs.first().and_then(|config| config.threads) {
        Some(threads) => threads,
        None => match std::env::var_os("RUST_TEST_THREADS") {
            Some(n) => n
                .to_str()
                .ok_or_else(|| eyre!("could not parse RUST_TEST_THREADS env var"))?
                .parse()?,
            None => std::thread::available_parallelism()?,
        },
    };

    run_and_collect(
        num_threads,
        |submit| {
            let mut todo = VecDeque::new();
            for config in &configs {
                todo.push_back((config.root_dir.clone(), config));
            }
            while let Some((path, config)) = todo.pop_front() {
                if path.is_dir() {
                    if path.file_name().unwrap() == "auxiliary" {
                        continue;
                    }
                    // Enqueue everything inside this directory.
                    // We want it sorted, to have some control over scheduling of slow tests.
                    let mut entries = std::fs::read_dir(path)
                        .unwrap()
                        .map(|e| e.unwrap().path())
                        .collect::<Vec<_>>();
                    entries.sort_by(|a, b| a.file_name().cmp(&b.file_name()));
                    for entry in entries {
                        todo.push_back((entry, config));
                    }
                } else if file_filter(&path, config) {
                    let status = status_emitter.register_test(path);
                    // Forward .rs files to the test workers.
                    submit.send((status, config)).unwrap();
                }
            }
        },
        |receive, finished_files_sender| -> Result<()> {
            for (status, config) in receive {
                let path = status.path();
                let file_contents = std::fs::read(path).unwrap();
                let mut config = config.clone();
                per_file_config(&mut config, path, &file_contents);
                let result = match std::panic::catch_unwind(|| {
                    parse_and_test_file(&build_manager, &status, config, file_contents)
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
                                stdout: vec![],
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
            stdout,
        },
    ) in &failures
    {
        let _guard = status.failed_test(command, stderr, stdout);
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
    num_threads: NonZeroUsize,
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
        for _ in 0..num_threads.get() {
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
    mut config: Config,
    file_contents: Vec<u8>,
) -> Result<Vec<TestRun>, Errored> {
    let comments = parse_comments(&file_contents)?;
    const EMPTY: &[String] = &[String::new()];
    // Run the test for all revisions
    let revisions = comments.revisions.as_deref().unwrap_or(EMPTY);
    let mut built_deps = false;
    Ok(revisions
        .iter()
        .map(|revision| {
            let status = status.for_revision(revision);
            // Ignore file if only/ignore rules do (not) apply
            if !status.test_file_conditions(&comments, &config) {
                return TestRun {
                    result: Ok(TestOk::Ignored),
                    status,
                };
            }

            if !built_deps {
                status.update_status("waiting for dependencies to finish building".into());
                match build_manager.build(Build::Dependencies, &config) {
                    Ok(extra_args) => config.program.args.extend(extra_args),
                    Err(err) => {
                        return TestRun {
                            result: Err(err),
                            status,
                        }
                    }
                }
                status.update_status(String::new());
                built_deps = true;
            }

            let result = status.run_test(build_manager, &config, &comments);
            TestRun { result, status }
        })
        .collect())
}

fn parse_comments(file_contents: &[u8]) -> Result<Comments, Errored> {
    match Comments::parse(file_contents) {
        Ok(comments) => Ok(comments),
        Err(errors) => Err(Errored {
            command: Command::new("parse comments"),
            errors,
            stderr: vec![],
            stdout: vec![],
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
    let file_contents = std::fs::read(aux_file).map_err(|err| Errored {
        command: Command::new(format!("reading aux file `{}`", aux_file.display())),
        errors: vec![],
        stderr: err.to_string().into_bytes(),
        stdout: vec![],
    })?;
    let comments = parse_comments(&file_contents)?;
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

    default_per_file_config(&mut config, aux_file, &file_contents);

    // Put aux builds into a separate directory per path so that multiple aux files
    // from different directories (but with the same file name) don't collide.
    let relative = strip_path_prefix(aux_file.parent().unwrap(), &config.out_dir);

    config.out_dir.extend(relative);

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
            stdout: output.stdout,
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
        let path = self.path();
        let revision = self.revision();

        let extra_args = build_aux_files(
            &path.parent().unwrap().join("auxiliary"),
            comments,
            revision,
            config,
            build_manager,
        )?;

        let mut config = config.clone();

        // Put aux builds into a separate directory per path so that multiple aux files
        // from different directories (but with the same file name) don't collide.
        let relative = strip_path_prefix(path.parent().unwrap(), &config.out_dir);

        config.out_dir.extend(relative);

        let mut cmd = build_command(path, &config, revision, comments)?;
        cmd.args(&extra_args);
        let stdin = path.with_extension(if revision.is_empty() {
            "stdin".into()
        } else {
            format!("{revision}.stdin")
        });
        if stdin.exists() {
            cmd.stdin(std::fs::File::open(stdin).unwrap());
        }

        let (cmd, status, stderr, stdout) = self.run_command(cmd)?;

        let mode = config.mode.maybe_override(comments, revision)?;
        let cmd = check_test_result(
            cmd,
            match *mode {
                Mode::Run { .. } => Mode::Pass,
                _ => *mode,
            },
            path,
            &config,
            revision,
            comments,
            status,
            &stdout,
            &stderr,
        )?;

        if let Mode::Run { .. } = *mode {
            return run_test_binary(mode, path, revision, comments, cmd, &config);
        }

        run_rustfix(
            &stderr, &stdout, path, comments, revision, &config, *mode, extra_args,
        )?;
        Ok(TestOk::Ok)
    }

    /// Run a command, and if it takes more than 100ms, start appending the last stderr/stdout
    /// line to the current status spinner.
    fn run_command(
        &self,
        mut cmd: Command,
    ) -> Result<(Command, ExitStatus, Vec<u8>, Vec<u8>), Errored> {
        match cmd.output() {
            Err(err) => Err(Errored {
                errors: vec![],
                stderr: err.to_string().into_bytes(),
                stdout: format!("could not spawn `{:?}` as a process", cmd.get_program())
                    .into_bytes(),
                command: cmd,
            }),
            Ok(Output {
                status,
                stdout,
                stderr,
            }) => Ok((cmd, status, stderr, stdout)),
        }
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
                            aux_file: strip_path_prefix(
                                &aux_file.canonicalize().map_err(|err| Errored {
                                    command: Command::new(format!(
                                        "canonicalizing path `{}`",
                                        aux_file.display()
                                    )),
                                    errors: vec![],
                                    stderr: err.to_string().into_bytes(),
                                    stdout: vec![],
                                })?,
                                &std::env::current_dir().unwrap(),
                            )
                            .collect(),
                        },
                        config,
                    )
                    .map_err(
                        |Errored {
                             command,
                             errors,
                             stderr,
                             stdout,
                         }| Errored {
                            command,
                            errors: vec![Error::Aux {
                                path: aux_file,
                                errors,
                                line,
                            }],
                            stderr,
                            stdout,
                        },
                    )?,
            );
        }
    }
    Ok(extra_args)
}

fn run_test_binary(
    mode: MaybeSpanned<Mode>,
    path: &Path,
    revision: &str,
    comments: &Comments,
    mut cmd: Command,
    config: &Config,
) -> TestResult {
    let revision = if revision.is_empty() {
        "run".to_string()
    } else {
        format!("run.{revision}")
    };
    cmd.arg("--print").arg("file-names");
    let output = cmd.output().unwrap();
    assert!(output.status.success());

    let mut files = output.stdout.lines();
    let file = files.next().unwrap();
    assert_eq!(files.next(), None);
    let file = std::str::from_utf8(file).unwrap();
    let exe_file = config.out_dir.join(file);
    let mut exe = Command::new(&exe_file);
    let stdin = path.with_extension(format!("{revision}.stdin"));
    if stdin.exists() {
        exe.stdin(std::fs::File::open(stdin).unwrap());
    }
    let output = exe
        .output()
        .unwrap_or_else(|err| panic!("exe file: {}: {err}", exe_file.display()));

    let mut errors = vec![];

    check_test_output(
        path,
        &mut errors,
        &revision,
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
            stdout: vec![],
        })
    }
}

fn run_rustfix(
    stderr: &[u8],
    stdout: &[u8],
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
        Mode::Pass | Mode::Run { .. } | Mode::Panic => RustfixMode::Disabled,
        Mode::Fail { rustfix, .. } | Mode::Yolo { rustfix } => rustfix,
    };

    let fixed_code = (no_run_rustfix.is_none() && global_rustfix.enabled())
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
                        if global_rustfix == RustfixMode::Everything {
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
            stdout: stdout.into(),
        })?;

    let edition = comments.edition(revision, config)?;
    let edition = edition
        .map(|mwl| {
            let line = mwl.span().unwrap_or(Span::INVALID);
            Spanned::new(mwl.into_inner(), line)
        })
        .into();
    let rustfix_comments = Comments {
        revisions: None,
        revisioned: std::iter::once((
            vec![],
            Revisioned {
                span: Span::INVALID,
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
                normalize_stdout: vec![],
                error_in_other_files: vec![],
                error_matches: vec![],
                require_annotations_for_level: Default::default(),
                aux_builds: comments
                    .for_revision(revision)
                    .flat_map(|r| r.aux_builds.iter().cloned())
                    .collect(),
                edition,
                mode: OptWithLine::new(Mode::Pass, Span::INVALID),
                no_rustfix: OptWithLine::new((), Span::INVALID),
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
        "fixed",
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
            stdout: vec![],
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
            stdout: output.stdout,
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
    stdout: &[u8],
    stderr: &[u8],
) -> Result<Command, Errored> {
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
        stdout,
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
        Ok(command)
    } else {
        Err(Errored {
            command,
            errors,
            stderr: diagnostics.rendered,
            stdout: stdout.into(),
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
        "stderr",
        &config.stderr_filters,
        config,
        comments,
        revision,
    );
    check_output(
        stdout,
        path,
        errors,
        "stdout",
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

    let mut seen_error_match = None;
    for error_pattern in error_patterns {
        seen_error_match = Some(error_pattern.span());
        // first check the diagnostics messages outside of our file. We check this first, so that
        // you can mix in-file annotations with //@error-in-other-file annotations, even if there is overlap
        // in the messages.
        if let Some(i) = messages_from_unknown_file_or_line
            .iter()
            .position(|msg| error_pattern.matches(&msg.message))
        {
            messages_from_unknown_file_or_line.remove(i);
        } else {
            errors.push(Error::PatternNotFound {
                pattern: error_pattern.clone(),
                expected_line: None,
            });
        }
    }

    // The order on `Level` is such that `Error` is the highest level.
    // We will ensure that *all* diagnostics of level at least `lowest_annotation_level`
    // are matched.
    let mut lowest_annotation_level = Level::Error;
    for &ErrorMatch {
        ref pattern,
        level,
        line,
    } in comments
        .for_revision(revision)
        .flat_map(|r| r.error_matches.iter())
    {
        seen_error_match = Some(pattern.span());
        // If we found a diagnostic with a level annotation, make sure that all
        // diagnostics of that level have annotations, even if we don't end up finding a matching diagnostic
        // for this pattern.
        if lowest_annotation_level > level {
            lowest_annotation_level = level;
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

        errors.push(Error::PatternNotFound {
            pattern: pattern.clone(),
            expected_line: Some(line),
        });
    }

    let required_annotation_level = comments.find_one_for_revision(
        revision,
        "`require_annotations_for_level` annotations",
        |r| r.require_annotations_for_level,
    )?;

    let required_annotation_level =
        required_annotation_level.map_or(lowest_annotation_level, |l| *l);
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
                    path: Some(Spanned::new(
                        path.to_path_buf(),
                        Span {
                            line_start: line,
                            ..Span::INVALID
                        },
                    )),
                    msgs,
                });
            }
        }
    }

    match (*mode, seen_error_match) {
        (Mode::Pass, Some(span)) | (Mode::Panic, Some(span)) => {
            errors.push(Error::PatternFoundInPassTest {
                mode: mode.span(),
                span,
            })
        }
        (
            Mode::Fail {
                require_patterns: true,
                ..
            },
            None,
        ) => errors.push(Error::NoPatternsFound),
        _ => {}
    }
    Ok(())
}

fn check_output(
    output: &[u8],
    path: &Path,
    errors: &mut Errors,
    kind: &'static str,
    filters: &Filter,
    config: &Config,
    comments: &Comments,
    revision: &str,
) -> PathBuf {
    let target = config.target.as_ref().unwrap();
    let output = normalize(path, output, filters, comments, revision, kind);
    let path = output_path(path, comments, revised(revision, kind), target, revision);
    match &config.output_conflict_handling {
        OutputConflictHandling::Error(bless_command) => {
            let expected_output = std::fs::read(&path).unwrap_or_default();
            if output != expected_output {
                errors.push(Error::OutputDiffers {
                    path: path.clone(),
                    actual: output.clone(),
                    expected: expected_output,
                    bless_command: bless_command.clone(),
                });
            }
        }
        OutputConflictHandling::Bless => {
            if output.is_empty() {
                let _ = std::fs::remove_file(&path);
            } else {
                std::fs::write(&path, &output).unwrap();
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
            return config.run_only_ignored;
        }
        if comments
            .for_revision(revision)
            .any(|r| r.needs_asm_support && !config.has_asm_support())
        {
            return config.run_only_ignored;
        }
        comments
            .for_revision(revision)
            .flat_map(|r| r.only.iter())
            .all(|c| test_condition(c, config))
            ^ config.run_only_ignored
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
    kind: &'static str,
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

    for (from, to) in comments.for_revision(revision).flat_map(|r| match kind {
        "fixed" => &[] as &[_],
        "stderr" => &r.normalize_stderr,
        "stdout" => &r.normalize_stdout,
        _ => unreachable!(),
    }) {
        text = from.replace_all(&text, to).into_owned();
    }
    text
}
/// Remove the common prefix of this path and the `root_dir`.
fn strip_path_prefix<'a>(path: &'a Path, prefix: &Path) -> impl Iterator<Item = Component<'a>> {
    let mut components = path.components();
    for c in prefix.components() {
        // Windows has some funky paths. This is probably wrong, but works well in practice.
        let deverbatimize = |c| match c {
            Component::Prefix(prefix) => Err(match prefix.kind() {
                Prefix::VerbatimUNC(a, b) => Prefix::UNC(a, b),
                Prefix::VerbatimDisk(d) => Prefix::Disk(d),
                other => other,
            }),
            c => Ok(c),
        };
        let c2 = components.next();
        if Some(deverbatimize(c)) == c2.map(deverbatimize) {
            continue;
        }
        return c2.into_iter().chain(components);
    }
    None.into_iter().chain(components)
}
