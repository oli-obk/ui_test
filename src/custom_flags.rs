//! Define custom test flags not natively supported by ui_test

use std::{
    panic::{RefUnwindSafe, UnwindSafe},
    process::{Command, Output},
};

use crate::{per_test_config::TestConfig, Config, Errored};

pub mod run;
pub mod rustfix;

/// Tester-specific flag that gets parsed from `//@` comments.
pub trait Flag: Send + Sync + UnwindSafe + RefUnwindSafe + std::fmt::Debug {
    /// Clone the boxed value and create a new box.
    fn clone_inner(&self) -> Box<dyn Flag>;

    /// Modify a command to what the flag specifies
    fn apply(&self, _cmd: &mut Command) {}

    /// Whether this flag causes a test to be filtered out
    fn test_condition(&self, _config: &Config) -> bool {
        false
    }

    /// Run an action after a test is finished.
    /// Returns the `cmd` back if no action was taken.
    fn post_test_action(
        &self,
        _config: &TestConfig<'_>,
        cmd: Command,
        _output: &Output,
    ) -> Result<Option<Command>, Errored> {
        Ok(Some(cmd))
    }
}

/// Use the unit type for when you don't need any behaviour and just need to know if the flag was set or not.
impl Flag for () {
    fn clone_inner(&self) -> Box<dyn Flag> {
        Box::new(())
    }
}

impl Clone for Box<dyn Flag> {
    fn clone(&self) -> Self {
        self.clone_inner()
    }
}
