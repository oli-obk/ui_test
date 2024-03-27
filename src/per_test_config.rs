//! This module allows you to configure the default settings for all
//! tests. All data structures here are normally parsed from `@` comments
//! in the files. These comments still overwrite the defaults, although
//! some boolean settings have no way to disable them.

use std::path::Path;

pub use crate::parser::{Comments, Condition, Revisioned};
pub use crate::rustc_stderr::Level;
use crate::Config;

pub(crate) struct TestConfig<'a> {
    pub config: Config,
    pub revision: &'a str,
    pub comments: &'a Comments,
    pub path: &'a Path,
}
