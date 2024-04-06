//! All the logic needed to run rustfix on a test that failed compilation

use std::{
    collections::HashSet,
    process::{Command, Output},
};

use spanned::Span;

use crate::{
    parser::OptWithLine,
    per_test_config::{Comments, Revisioned, TestConfig},
    rustc_stderr, Error, Errored, Mode,
};

use super::Flag;

/// When to run rustfix on tests
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RustfixMode {
    /// Do not run rustfix on the test
    Disabled,
    /// Apply only `MachineApplicable` suggestions emitted by the test
    MachineApplicable,
    /// Apply all suggestions emitted by the test
    Everything,
}

impl RustfixMode {
    pub(crate) fn enabled(self) -> bool {
        self != RustfixMode::Disabled
    }
}

impl Flag for RustfixMode {
    fn clone_inner(&self) -> Box<dyn Flag> {
        Box::new(*self)
    }
    fn post_test_action(
        &self,
        config: &TestConfig<'_>,
        cmd: Command,
        output: &Output,
    ) -> Result<Option<Command>, Errored> {
        let global_rustfix = match *config.mode()? {
            Mode::Pass | Mode::Panic => RustfixMode::Disabled,
            Mode::Fail { .. } | Mode::Yolo => *self,
        };

        let output = output.clone();
        let no_run_rustfix = config.find_one_custom("no-rustfix")?;

        let fixed_code = (no_run_rustfix.is_none() && global_rustfix.enabled())
            .then_some(())
            .and_then(|()| {
                let suggestions = std::str::from_utf8(&output.stderr)
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
                    let path_str = config.path.display().to_string();
                    for sugg in &suggestions {
                        for snip in &sugg.snippets {
                            if snip.file_name != path_str {
                                return Some(Err(anyhow::anyhow!("cannot apply suggestions for `{}` since main file is `{path_str}`. Please use `//@no-rustfix` to disable rustfix", snip.file_name)));
                            }
                        }
                    }
                    Some(rustfix::apply_suggestions(
                        &std::fs::read_to_string(config.path).unwrap(),
                        &suggestions,
                    ))
                }
            })
            .transpose()
            .map_err(|err| Errored {
                command: Command::new(format!("rustfix {}", config.path.display())),
                errors: vec![Error::Rustfix(err)],
                stderr: output.stderr,
                stdout: output.stdout,
            })?;

        let rustfix_comments = Comments {
            revisions: None,
            revisioned: std::iter::once((
                vec![],
                Revisioned {
                    span: Span::default(),
                    ignore: vec![],
                    only: vec![],
                    stderr_per_bitwidth: false,
                    compile_flags: config.collect(|r| r.compile_flags.iter().cloned()),
                    env_vars: config.collect(|r| r.env_vars.iter().cloned()),
                    normalize_stderr: vec![],
                    normalize_stdout: vec![],
                    error_in_other_files: vec![],
                    error_matches: vec![],
                    require_annotations_for_level: Default::default(),
                    aux_builds: config.collect(|r| r.aux_builds.iter().cloned()),
                    mode: OptWithLine::new(Mode::Pass, Span::default()),
                    diagnostic_code_prefix: OptWithLine::new(String::new(), Span::default()),
                    custom: config
                        .comments
                        .for_revision(config.revision)
                        .flat_map(|r| r.custom.clone())
                        .collect(),
                },
            ))
            .collect(),
        };
        let config = TestConfig {
            config: config.config.clone(),
            revision: config.revision,
            comments: &rustfix_comments,
            path: config.path,
        };

        let run = fixed_code.is_some();
        let mut errors = vec![];
        let rustfix_path = config.check_output(
            // Always check for `.fixed` files, even if there were reasons not to run rustfix.
            // We don't want to leave around stray `.fixed` files
            fixed_code.unwrap_or_default().as_bytes(),
            &mut errors,
            "fixed",
        );
        // picking the crate name from the file name is problematic when `.revision_name` is inserted,
        // so we compute it here before replacing the path.
        let crate_name = config
            .path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('-', "_");
        let config = TestConfig {
            config: config.config,
            revision: config.revision,
            comments: &rustfix_comments,
            path: &rustfix_path,
        };
        if !errors.is_empty() {
            return Err(Errored {
                command: Command::new(format!("checking {}", config.path.display())),
                errors,
                stderr: vec![],
                stdout: vec![],
            });
        }

        if !run {
            return Ok(Some(cmd));
        }

        let mut cmd = config.build_command()?;
        cmd.arg("--crate-name").arg(crate_name);
        let output = cmd.output().unwrap();
        if output.status.success() {
            Ok(None)
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
}
