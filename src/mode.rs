use spanned::Spanned;

use super::Error;
use std::fmt::Display;
use std::process::ExitStatus;

#[derive(Copy, Clone, Debug)]
/// Decides what is expected of each test's exit status.
pub enum Mode {
    /// The test passes a full execution of the rustc driver
    Pass,
    /// The rustc driver panicked
    Panic,
    /// The rustc driver emitted an error
    Fail {
        /// Whether failing tests must have error patterns. Set to false if you just care about .stderr output.
        require_patterns: bool,
    },
    /// Run the tests, but always pass them as long as all annotations are satisfied and stderr files match.
    Yolo,
}

impl Mode {
    #[allow(clippy::result_large_err)]
    pub(crate) fn ok(self, status: ExitStatus) -> Result<(), Error> {
        let expected = match self {
            Mode::Pass => 0,
            Mode::Panic => 101,
            Mode::Fail { .. } => 1,
            Mode::Yolo { .. } => return Ok(()),
        };
        if status.code() == Some(expected) {
            Ok(())
        } else {
            Err(Error::ExitStatus {
                mode: self.to_string(),
                status,
                expected,
                reason: Spanned::dummy(
                    match (expected, status.code()) {
                        (_, Some(101)) => "the compiler panicked",
                        (0, Some(1)) => "compilation failed, but was expected to succeed",
                        (1, Some(0)) => "compilation succeeded, but was expected to fail",
                        _ => "",
                    }
                    .into(),
                ),
            })
        }
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Pass => write!(f, "pass"),
            Mode::Panic => write!(f, "panic"),
            Mode::Fail {
                require_patterns: _,
            } => write!(f, "fail"),
            Mode::Yolo => write!(f, "yolo"),
        }
    }
}
