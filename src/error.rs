use crate::{
    parser::{OptWithLine, Pattern, WithLine},
    rustc_stderr::Message,
    Mode,
};
use std::{path::PathBuf, process::ExitStatus};

/// All the ways in which a test can fail.
#[derive(Debug)]
#[must_use]
pub enum Error {
    /// Got an invalid exit status for the given mode.
    ExitStatus {
        /// The expected mode.
        mode: Mode,
        /// The exit status of the command.
        status: ExitStatus,
        /// The expected exit status as set in the file or derived from the mode.
        expected: i32,
    },
    /// A pattern was declared but had no matching error.
    PatternNotFound(WithLine<Pattern>),
    /// A ui test checking for failure does not have any failure patterns
    NoPatternsFound,
    /// A ui test checking for success has failure patterns
    PatternFoundInPassTest,
    /// Stderr/Stdout differed from the `.stderr`/`.stdout` file present.
    OutputDiffers {
        /// The file containing the expected output that differs from the actual output.
        path: PathBuf,
        /// The output from the command.
        actual: Vec<u8>,
        /// The contents of the file.
        expected: Vec<u8>,
        /// A command, that when run, causes the output to get blessed instead of erroring.
        bless_command: String,
    },
    /// There were errors that don't have a pattern.
    ErrorsWithoutPattern {
        /// The main message of the error.
        msgs: Vec<Message>,
        /// File and line information of the error.
        path: OptWithLine<PathBuf>,
    },
    /// A comment failed to parse.
    InvalidComment {
        /// The comment
        msg: String,
        /// THe line in which it was defined.
        line: usize,
    },
    /// A subcommand (e.g. rustfix) of a test failed.
    Command {
        /// The name of the subcommand (e.g. "rustfix").
        kind: String,
        /// The exit status of the command.
        status: ExitStatus,
    },
    /// This catches crashes of ui tests and reports them along the failed test.
    Bug(String),
    /// An auxiliary build failed with its own set of errors.
    Aux {
        /// Path to the aux file.
        path: PathBuf,
        /// The errors that occurred during the build of the aux file.
        errors: Vec<Error>,
        /// The line in which the aux file was requested to be built.
        line: usize,
    },
    /// An error occured applying [`rustfix`] suggestions
    Rustfix(anyhow::Error),
}

pub(crate) type Errors = Vec<Error>;
