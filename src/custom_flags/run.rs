//! Types used for running tests after they pass compilation

use std::process::{Command, Output};

use crate::{per_test_config::TestConfig, Errored};

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
    ) -> Result<Option<Command>, Errored> {
        config.run_test_binary(cmd, self.exit_code)?;
        Ok(None)
    }
}
