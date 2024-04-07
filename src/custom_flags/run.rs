//! Types used for running tests after they pass compilation

use bstr::ByteSlice;
use std::process::{Command, Output};

use crate::{build_manager::BuildManager, per_test_config::TestConfig, Error, Errored};

use super::Flag;

#[derive(Debug, Copy, Clone)]
pub(crate) struct Run {
    pub exit_code: i32,
}

impl Flag for Run {
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
            revision: &revision,
            comments: config.comments,
            path: config.path,
            aux_dir: config.aux_dir,
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
        let stdin = config.path.with_extension(format!("{revision}.stdin"));
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
            })
        }
        if errors.is_empty() {
            Ok(None)
        } else {
            Err(Errored {
                command: exe,
                errors,
                stderr: vec![],
                stdout: vec![],
            })
        }
    }
}
