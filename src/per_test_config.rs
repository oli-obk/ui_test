//! This module allows you to configure the default settings for all
//! tests. All data structures here are normally parsed from `@` comments
//! in the files. These comments still overwrite the defaults, although
//! some boolean settings have no way to disable them.

pub use crate::parser::{Comments, Condition, Revisioned};
pub use crate::rustc_stderr::Level;
