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
use per_test_config::TestConfig;
use rustc_stderr::Message;
use status_emitter::{StatusEmitter, TestStatus};
use std::collections::VecDeque;
use std::ffi::OsString;
use std::num::NonZeroUsize;
use std::path::{Component, Path, Prefix};
use std::process::{Command, Output};
use std::thread;
use test_result::{Errored, TestOk, TestRun};

use crate::parser::Comments;

mod cmd;
mod config;
mod dependencies;
mod diff;
mod error;
pub mod filter;
pub mod github_actions;
mod mode;
mod parser;
pub mod per_test_config;
mod rustc_stderr;
pub mod status_emitter;
pub mod test_result;

#[cfg(test)]
mod tests;

pub use cmd::*;
pub use config::*;
pub use error::*;
pub use mode::*;

pub use spanned;

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
    config.with_args(&args);

    run_tests_generic(
        vec![config],
        default_file_filter,
        default_per_file_config,
        (text, status_emitter::Gha::<true> { name }),
    )
}

/// The filter used by `run_tests` to only run on `.rs` files that are
/// specified by [`Config::filter_files`] and [`Config::skip_files`].
/// Returns `None` if there is no extension or the extension is not `.rs`.
pub fn default_file_filter(path: &Path, config: &Config) -> Option<bool> {
    path.extension().filter(|&ext| ext == "rs")?;
    Some(default_any_file_filter(path, config))
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
    config.program.args.push(
        match crate_type(file_contents) {
            CrateType::ProcMacro => "--crate-type=proc-macro",
            CrateType::Test => "--test",
            CrateType::Bin => return,
            CrateType::Lib => "--crate-type=lib",
        }
        .into(),
    )
}

/// The kind of crate we're building here. Corresponds to `--crate-type` flags of rustc
pub enum CrateType {
    /// A proc macro
    ProcMacro,
    /// A file containing unit tests
    Test,
    /// A binary file containing a main function or start function
    Bin,
    /// A library crate
    Lib,
}

/// Heuristic:
/// * if the file contains `#[test]`, automatically pass `--cfg test`.
/// * if the file does not contain `fn main()` or `#[start]`, automatically pass `--crate-type=lib`.
/// This avoids having to spam `fn main() {}` in almost every test.
pub fn crate_type(file_contents: &[u8]) -> CrateType {
    if file_contents.find(b"#[proc_macro").is_some() {
        CrateType::ProcMacro
    } else if file_contents.find(b"#[test]").is_some() {
        CrateType::Test
    } else if file_contents.find(b"fn main()").is_none()
        && file_contents.find(b"#[start]").is_none()
    {
        CrateType::Lib
    } else {
        CrateType::Bin
    }
}

/// Create a command for running a single file, with the settings from the `config` argument.
/// Ignores various settings from `Config` that relate to finding test files.
pub fn test_command(mut config: Config, path: &Path) -> Result<Command> {
    config.fill_host_and_target()?;
    let extra_args = config.build_dependencies()?;

    let comments = Comments::parse_file(config.comment_defaults.clone(), path)?
        .map_err(|errors| color_eyre::eyre::eyre!("{errors:#?}"))?;
    let config = TestConfig {
        config,
        revision: "",
        comments: &comments,
        path,
    };
    let mut result = config.build_command().unwrap();
    result.args(extra_args);

    Ok(result)
}

/// A version of `run_tests` that allows more fine-grained control over running tests.
///
/// All `configs` are being run in parallel.
/// If multiple configs are provided, the [`Config::threads`] value of the first one is used;
/// the thread count of all other configs is ignored.
/// The file filter is supposed to return `None` if it was filtered because of file extensions
/// and `Some(false)` if the file was rejected out of other reasons like the file path not matching
/// a user defined filter.
pub fn run_tests_generic(
    mut configs: Vec<Config>,
    file_filter: impl Fn(&Path, &Config) -> Option<bool> + Sync,
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

    let mut filtered = 0;
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
                } else if let Some(matched) = file_filter(&path, config) {
                    if matched {
                        let status = status_emitter.register_test(path);
                        // Forward .rs files to the test workers.
                        submit.send((status, config)).unwrap();
                    } else {
                        filtered += 1;
                    }
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

    for run in results {
        match run.result {
            Ok(TestOk::Ok) => succeeded += 1,
            Ok(TestOk::Ignored) => ignored += 1,
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
    let comments = Comments::parse(
        &file_contents,
        config.comment_defaults.clone(),
        status.path(),
    )
    .map_err(|errors| Errored::new(errors, "parse comments"))?;
    const EMPTY: &[String] = &[String::new()];
    // Run the test for all revisions
    let revisions = comments.revisions.as_deref().unwrap_or(EMPTY);
    let mut built_deps = false;
    Ok(revisions
        .iter()
        .map(|revision| {
            let status = status.for_revision(revision);
            // Ignore file if only/ignore rules do (not) apply
            if !config.test_file_conditions(&comments, revision) {
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

            let test_config = TestConfig {
                config: config.clone(),
                revision,
                comments: &comments,
                path: status.path(),
            };

            let result = test_config.run_test(build_manager);
            TestRun { result, status }
        })
        .collect())
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
    let comments = Comments::parse(&file_contents, config.comment_defaults.clone(), aux_file)
        .map_err(|errors| Errored::new(errors, "parse aux comments"))?;
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

    match crate_type(&file_contents) {
        // Proc macros must be run on the host
        CrateType::ProcMacro => config.target = config.host.clone(),
        CrateType::Test | CrateType::Bin | CrateType::Lib => {}
    }

    let mut config = TestConfig {
        config,
        revision: "",
        comments: &comments,
        path: aux_file,
    };

    config.patch_out_dir();

    let mut aux_cmd = config.build_command()?;

    let mut extra_args = config.build_aux_files(aux_file.parent().unwrap(), build_manager)?;
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
        let path = config.config.out_dir.join(file);
        extra_args.push("--extern".into());
        let mut cname = OsString::from(&crate_name);
        cname.push("=");
        cname.push(path);
        extra_args.push(cname);
        // Help cargo find the crates added with `--extern`.
        extra_args.push("-L".into());
        extra_args.push(config.config.out_dir.as_os_str().to_os_string());
    }
    Ok(extra_args)
}

fn run_command(mut cmd: Command) -> Result<(Command, Output), Errored> {
    match cmd.output() {
        Err(err) => Err(Errored {
            errors: vec![],
            stderr: err.to_string().into_bytes(),
            stdout: format!("could not spawn `{:?}` as a process", cmd.get_program()).into_bytes(),
            command: cmd,
        }),
        Ok(output) => Ok((cmd, output)),
    }
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
