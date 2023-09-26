use crate::{
    parser::{Pattern, Spanned},
    rustc_stderr::{Message, Span},
    Mode,
};
use std::{num::NonZeroUsize, path::PathBuf, process::ExitStatus};

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
    PatternNotFound {
        /// The pattern that was not found, and the span of where that pattern was declared.
        pattern: Spanned<Pattern>,
        /// Can be `None` when it is expected outside the current file
        expected_line: Option<NonZeroUsize>,
    },
    /// A ui test checking for failure does not have any failure patterns
    NoPatternsFound,
    /// A ui test checking for success has failure patterns
    PatternFoundInPassTest {
        /// Span of a flag changing the mode (if changed from default).
        mode: Option<Span>,
        /// Span of the pattern
        span: Span,
    },
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
        path: Option<Spanned<PathBuf>>,
    },
    /// A comment failed to parse.
    InvalidComment {
        /// The comment
        msg: String,
        /// The character range in which it was defined.
        span: Span,
    },
    /// Conflicting comments
    MultipleRevisionsWithResults {
        /// The comment being looked for
        kind: String,
        /// The lines where conflicts happened
        lines: Vec<NonZeroUsize>,
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
        line: NonZeroUsize,
    },
    /// An error occured applying [`rustfix`] suggestions
    Rustfix(anyhow::Error),
}

pub(crate) type Errors = Vec<Error>;
