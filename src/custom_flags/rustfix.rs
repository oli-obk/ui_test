//! All the logic needed to run rustfix on a test that failed compilation

use std::{
    collections::HashSet,
    process::{Command, Output},
};

use spanned::{Span, Spanned};

use crate::{
    build_manager::BuildManager,
    display,
    parser::OptWithLine,
    per_test_config::{Comments, Revisioned, TestConfig},
    test_result::TestRun,
    Error, Errored, TestOk,
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
    fn must_be_unique(&self) -> bool {
        true
    }
    fn post_test_action(
        &self,
        config: &TestConfig<'_>,
        _cmd: &mut Command,
        output: &Output,
        build_manager: &BuildManager<'_>,
    ) -> Result<Option<TestRun>, Errored> {
        let global_rustfix = match config.exit_status()? {
            Some(Spanned {
                content: 101 | 0, ..
            }) => RustfixMode::Disabled,
            _ => *self,
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
                    let path_str = display(config.status.path());
                    for sugg in &suggestions {
                        for snip in &sugg.snippets {
                            let file_name = snip.file_name.replace('\\', "/");
                            if file_name != path_str {
                                return Some(Err(anyhow::anyhow!("cannot apply suggestions for `{file_name}` since main file is `{path_str}`. Please use `//@no-rustfix` to disable rustfix")));
                            }
                        }
                    }
                    Some(rustfix::apply_suggestions(
                        &std::fs::read_to_string(config.status.path()).unwrap(),
                        &suggestions,
                    ).map_err(|e| e.into()))
                }
            })
            .transpose()
            .map_err(|err| Errored {
                command: format!("rustfix {}", display(config.status.path())),
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
                    diagnostic_code_prefix: OptWithLine::new(String::new(), Span::default()),
                    custom: config.comments().flat_map(|r| r.custom.clone()).collect(),
                    exit_status: OptWithLine::new(0, Span::default()),
                    require_annotations: OptWithLine::default(),
                },
            ))
            .collect(),
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
            .status
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('-', "_");

        if !errors.is_empty() {
            return Ok(Some(TestRun {
                result: Err(Errored {
                    command: format!("checking {}", display(config.status.path())),
                    errors,
                    stderr: vec![],
                    stdout: vec![],
                }),
                status: config.status.for_path(&rustfix_path),
            }));
        }

        if !run {
            return Ok(None);
        }

        let config = TestConfig {
            config: config.config.clone(),
            comments: &rustfix_comments,
            aux_dir: config.aux_dir,
            status: config.status.for_path(&rustfix_path),
        };

        let mut cmd = config.build_command(build_manager)?;
        cmd.arg("--crate-name").arg(crate_name);
        let output = cmd.output().unwrap();
        if output.status.success() {
            Ok(Some(TestRun {
                result: Ok(TestOk::Ok),
                status: config.status,
            }))
        } else {
            let diagnostics = config.process(&output.stderr);
            Err(Errored {
                command: format!("{cmd:?}"),
                errors: vec![Error::ExitStatus {
                    expected: 0,
                    status: output.status,
                    reason: Spanned::new(
                        "after rustfix is applied, all errors should be gone, but weren't".into(),
                        diagnostics
                            .messages
                            .iter()
                            .flatten()
                            .chain(diagnostics.messages_from_unknown_file_or_line.iter())
                            .find_map(|message| message.line_col.clone())
                            .unwrap_or_default(),
                    ),
                }],
                stderr: diagnostics.rendered,
                stdout: output.stdout,
            })
        }
    }
}
