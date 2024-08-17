//! Types used for running tests after they pass compilation

use bstr::ByteSlice;
use spanned::Spanned;
use std::process::{Command, Output};

use crate::{
    build_manager::BuildManager, display, per_test_config::TestConfig,
    status_emitter::RevisionStyle, Error, Errored, TestOk, TestRun,
};

use super::Flag;

#[derive(Debug, Copy, Clone)]
pub(crate) struct Run {
    pub exit_code: i32,
}

impl Flag for Run {
    fn must_be_unique(&self) -> bool {
        true
    }
    fn clone_inner(&self) -> Box<dyn Flag> {
        Box::new(*self)
    }

    fn post_test_action(
        &self,
        config: &TestConfig,
        _output: &Output,
        build_manager: &BuildManager,
    ) -> Result<(), Errored> {
        let mut cmd = config.build_command(build_manager)?;
        let exit_code = self.exit_code;
        let revision = config.extension("run");
        let config = TestConfig {
            config: config.config.clone(),
            comments: config.comments.clone(),
            aux_dir: config.aux_dir.clone(),
            status: config.status.for_revision(&revision, RevisionStyle::Show),
        };
        build_manager.add_new_job(move || {
            cmd.arg("--print").arg("file-names");
            let output = cmd.output().unwrap();
            if config.aborted() {
                return TestRun {
                    result: Err(Errored::aborted()),
                    status: config.status,
                    abort_check: config.config.abort_check.clone(),
                };
            }
            assert!(output.status.success(), "{cmd:#?}: {output:#?}");

            let mut files = output.stdout.lines();
            let file = files.next().unwrap();
            assert_eq!(files.next(), None);
            let file = std::str::from_utf8(file).unwrap();
            let exe_file = config.config.out_dir.join(file);
            let mut exe = Command::new(&exe_file);
            let stdin = config
                .status
                .path()
                .with_extension(format!("{revision}.stdin"));
            if stdin.exists() {
                exe.stdin(std::fs::File::open(stdin).unwrap());
            }
            let output = exe
                .output()
                .unwrap_or_else(|err| panic!("exe file: {}: {err}", display(&exe_file)));

            if config.aborted() {
                return TestRun {
                    result: Err(Errored::aborted()),
                    status: config.status,
                    abort_check: config.config.abort_check.clone(),
                };
            }

            let mut errors = vec![];

            config.check_test_output(&mut errors, &output.stdout, &output.stderr);

            let status = output.status;
            if status.code() != Some(exit_code) {
                errors.push(Error::ExitStatus {
                    status,
                    expected: exit_code,
                    reason: match (exit_code, status.code()) {
                        (_, Some(101)) => get_panic_span(&output.stderr),
                        (0, _) => {
                            Spanned::dummy("the test was expected to run successfully".into())
                        }
                        (101, _) => Spanned::dummy("the test was expected to panic".into()),
                        _ => Spanned::dummy(String::new()),
                    },
                })
            }

            TestRun {
                result: if errors.is_empty() {
                    Ok(TestOk::Ok)
                } else {
                    Err(Errored {
                        command: format!("{exe:?}"),
                        errors,
                        stderr: output.stderr,
                        stdout: output.stdout,
                    })
                },
                status: config.status,
                abort_check: config.config.abort_check,
            }
        });
        Ok(())
    }
}

fn get_panic_span(stderr: &[u8]) -> Spanned<String> {
    let mut lines = stderr.lines();
    while let Some(line) = lines.next() {
        if let Some((_, location)) = line.split_once_str(b"panicked at ") {
            let mut parts = location.split(|&c| c == b':');
            let Some(filename) = parts.next() else {
                continue;
            };
            let Some(line) = parts.next() else { continue };
            let Some(col) = parts.next() else { continue };
            let message = lines
                .next()
                .and_then(|msg| msg.to_str().ok())
                .unwrap_or("the test panicked during execution");
            let Ok(line) = line.to_str() else { continue };
            let Ok(col) = col.to_str() else { continue };
            let Ok(filename) = filename.to_str() else {
                continue;
            };
            let Ok(line) = line.parse::<usize>() else {
                continue;
            };
            let Ok(col) = col.parse::<usize>() else {
                continue;
            };
            let Ok(file) = Spanned::read_from_file(filename) else {
                continue;
            };
            let Some(line) = line.checked_sub(1) else {
                continue;
            };
            let Some(line) = file.lines().nth(line) else {
                continue;
            };
            let Some(col) = col.checked_sub(1) else {
                continue;
            };
            let span = line.span.inc_col_start(col);
            return Spanned::new(message.into(), span);
        }
    }
    Spanned::dummy("".into())
}
