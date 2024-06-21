//! All the logic needed to run rustfix on a test that failed compilation

use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    process::{Command, Output},
};

use rustfix::{CodeFix, Suggestion};
use spanned::{Span, Spanned};

use crate::{
    build_manager::BuildManager,
    parser::OptWithLine,
    per_test_config::{Comments, Revisioned, TestConfig},
    Error, Errored,
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

    fn filter(self) -> rustfix::Filter {
        if self == RustfixMode::Everything {
            rustfix::Filter::Everything
        } else {
            rustfix::Filter::MachineApplicableOnly
        }
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
        cmd: Command,
        output: &Output,
        build_manager: &BuildManager<'_>,
    ) -> Result<Option<Command>, Errored> {
        let global_rustfix = match config.exit_status()? {
            Some(Spanned {
                content: 101 | 0, ..
            }) => RustfixMode::Disabled,
            _ => *self,
        };
        let no_run_rustfix = config.find_one_custom("no-rustfix")?;

        let fixed = if no_run_rustfix.is_none() && global_rustfix.enabled() {
            fix(
                &output.stderr,
                config.status.path(),
                global_rustfix.filter(),
            )
            .map_err(|err| Errored {
                command: Command::new(format!("rustfix {}", config.status.path().display())),
                errors: vec![Error::Rustfix(err)],
                stderr: output.stderr.clone(),
                stdout: output.stdout.clone(),
            })?
        } else {
            Vec::new()
        };

        let mut errors = Vec::new();
        let fixed_paths = match fixed.as_slice() {
            [] => Vec::new(),
            [single] => {
                vec![config.check_output(single.as_bytes(), &mut errors, "fixed")]
            }
            _ => {
                config.check_output(&[], &mut errors, "fixed");
                fixed
                    .iter()
                    .enumerate()
                    .map(|(i, fix)| {
                        config.check_output(
                            fix.as_bytes(),
                            &mut errors,
                            &format!("{}.fixed", i + 1),
                        )
                    })
                    .collect()
            }
        };

        if fixed.len() != 1 {
            // Remove an unused .fixed file
            config.check_output(&[], &mut errors, "fixed");
        }

        if !errors.is_empty() {
            return Err(Errored {
                command: Command::new(format!("checking {}", config.status.path().display())),
                errors,
                stderr: vec![],
                stdout: vec![],
            });
        }

        compile_fixed(config, build_manager, &fixed_paths)?;

        if fixed.is_empty() {
            Ok(Some(cmd))
        } else {
            Ok(None)
        }
    }
}

fn fix(stderr: &[u8], path: &Path, filter: rustfix::Filter) -> anyhow::Result<Vec<String>> {
    let suggestions = std::str::from_utf8(stderr)
        .unwrap()
        .lines()
        .filter_map(|line| {
            if !line.starts_with('{') {
                return None;
            }
            let diagnostic = serde_json::from_str(line).unwrap_or_else(|err| {
                panic!("could not deserialize diagnostics json for rustfix {err}:{line}")
            });
            rustfix::collect_suggestions(&diagnostic, &HashSet::new(), filter)
        })
        .collect::<Vec<_>>();
    if suggestions.is_empty() {
        return Ok(Vec::new());
    }

    let max_solutions = suggestions
        .iter()
        .map(|suggestion| suggestion.solutions.len())
        .max()
        .unwrap();
    let src = std::fs::read_to_string(path).unwrap();
    let mut fixes = (0..max_solutions)
        .map(|_| CodeFix::new(&src))
        .collect::<Vec<_>>();
    for Suggestion {
        message,
        snippets,
        solutions,
    } in suggestions
    {
        for snippet in &snippets {
            anyhow::ensure!(
                Path::new(&snippet.file_name) == path,
                "cannot apply suggestions for `{}` since main file is `{}`. Please use `//@no-rustfix` to disable rustfix",
                snippet.file_name,
                path.display()
            );
        }

        let repeat_first = std::iter::from_fn(|| solutions.first());
        for (solution, fix) in solutions.iter().chain(repeat_first).zip(&mut fixes) {
            // TODO: use CodeFix::apply_solution when rustfix 0.8.5 is published
            fix.apply(&Suggestion {
                solutions: vec![solution.clone()],
                message: message.clone(),
                snippets: snippets.clone(),
            })?;
        }
    }

    fixes.into_iter().map(|fix| Ok(fix.finish()?)).collect()
}

fn compile_fixed(
    config: &TestConfig,
    build_manager: &BuildManager,
    fixed_paths: &[PathBuf],
) -> Result<(), Errored> {
    let crate_name = config
        .status
        .path()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .replace('-', "_");

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

    for fixed_path in fixed_paths {
        let fixed_config = TestConfig {
            config: config.config.clone(),
            comments: &rustfix_comments,
            aux_dir: config.aux_dir,
            status: &config.status.for_path(fixed_path),
        };
        let mut cmd = fixed_config.build_command(build_manager)?;
        cmd.arg("--crate-name").arg(&crate_name);
        let output = cmd.output().unwrap();
        if !output.status.success() {
            let diagnostics = fixed_config.process(&output.stderr);
            return Err(Errored {
                command: cmd,
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
            });
        }
    }

    Ok(())
}
