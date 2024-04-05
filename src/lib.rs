#![allow(
    clippy::enum_variant_names,
    clippy::useless_format,
    clippy::too_many_arguments,
    rustc::internal
)]
#![deny(missing_docs)]

//! A crate to run the Rust compiler (or other binaries) and test their command line output.

pub use color_eyre;
use color_eyre::eyre::eyre;
pub use color_eyre::eyre::Result;
pub use core::run_and_collect;
pub use core::CrateType;
use dependencies::{Build, BuildManager};
pub use filter::Match;
use per_test_config::TestConfig;
use rustc_stderr::Message;
use status_emitter::{StatusEmitter, TestStatus};
use std::collections::VecDeque;
use std::path::Path;
use std::process::Command;
use test_result::TestRun;
pub use test_result::{Errored, TestOk};

use crate::parser::Comments;

mod cmd;
mod config;
pub mod core;
mod dependencies;
mod diff;
mod error;
pub mod filter;
pub mod github_actions;
mod mode;
pub mod nextest;
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
        match CrateType::from_file_contents(file_contents) {
            CrateType::ProcMacro => "--crate-type=proc-macro",
            CrateType::Test => "--test",
            CrateType::Bin => return,
            CrateType::Lib => "--crate-type=lib",
        }
        .into(),
    )
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
    if nextest::emulate(&mut configs) {
        return Ok(());
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
    core::run_and_collect(
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
