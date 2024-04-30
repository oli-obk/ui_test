//! Types used for running tests after they pass compilation

use bstr::ByteSlice;
use spanned::{Span, Spanned};
use std::{
    path::PathBuf,
    process::{Command, Output},
};

use crate::{build_manager::BuildManager, per_test_config::TestConfig, Error, Errored};

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
        config: &TestConfig<'_>,
        cmd: Command,
        _output: &Output,
        _build_manager: &BuildManager<'_>,
    ) -> Result<Option<Command>, Errored> {
        let mut cmd = cmd;
        let exit_code = self.exit_code;
        let revision = config.extension("run");
        let config = TestConfig {
            config: config.config.clone(),
            comments: config.comments,
            aux_dir: config.aux_dir,
            status: &config.status.for_revision(&revision),
        };
        cmd.arg("--print").arg("file-names");
        let output = cmd.output().unwrap();
        assert!(output.status.success());

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
            .unwrap_or_else(|err| panic!("exe file: {}: {err}", exe_file.display()));

        let mut errors = vec![];

        config.check_test_output(&mut errors, &output.stdout, &output.stderr);

        let status = output.status;
        if status.code() != Some(exit_code) {
            errors.push(Error::ExitStatus {
                mode: format!("run({exit_code})"),
                status,
                expected: exit_code,
                reason: match (exit_code, status.code()) {
                    (_, Some(101)) => get_panic_span(&output.stderr),
                    (0, _) => Spanned::dummy("the test was expected to run successfully".into()),
                    (101, _) => Spanned::dummy("the test was expected to panic".into()),
                    _ => Spanned::dummy(String::new()),
                },
            })
        }
        if errors.is_empty() {
            Ok(None)
        } else {
            Err(Errored {
                command: exe,
                errors,
                stderr: output.stderr,
                stdout: output.stdout,
            })
        }
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
            let Ok(line) = line.parse() else {
                continue;
            };
            let Ok(col) = col.parse() else {
                continue;
            };
            let span = Span {
                file: PathBuf::from(filename),
                line_start: line,
                line_end: line,
                col_start: col,
                col_end: col,
            };
            return Spanned::new(message.into(), span);
        }
    }
    Spanned::dummy("".into())
}
