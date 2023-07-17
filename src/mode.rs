use super::Error;
use super::Errors;
use crate::parser::Comments;
use std::fmt::Display;
use std::process::ExitStatus;

#[derive(Copy, Clone, Debug)]
/// Decides what is expected of each test's exit status.
pub enum Mode {
    /// The test passes a full execution of the rustc driver
    Pass,
    /// The test produces an executable binary that can get executed on the host
    Run {
        /// The expected exit code
        exit_code: i32,
    },
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
    pub(crate) fn ok(self, status: ExitStatus) -> Errors {
        let expected = match self {
            Mode::Run { exit_code } => exit_code,
            Mode::Pass => 0,
            Mode::Panic => 101,
            Mode::Fail { .. } => 1,
            Mode::Yolo => return vec![],
        };
        if status.code() == Some(expected) {
            vec![]
        } else {
            vec![Error::ExitStatus {
                mode: self,
                status,
                expected,
            }]
        }
    }
    pub(crate) fn maybe_override(
        self,
        comments: &Comments,
        revision: &str,
        errors: &mut Vec<Error>,
    ) -> Self {
        comments
            .find_one_for_revision(
                revision,
                |r| r.mode.as_ref().cloned(),
                |line| {
                    errors.push(Error::InvalidComment {
                        msg: "multiple mode changes found".into(),
                        line,
                    })
                },
            )
            .map(|wl| *wl)
            .unwrap_or(self)
    }
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Run { exit_code } => write!(f, "run({exit_code})"),
            Mode::Pass => write!(f, "pass"),
            Mode::Panic => write!(f, "panic"),
            Mode::Fail {
                require_patterns: _,
            } => write!(f, "fail"),
            Mode::Yolo => write!(f, "yolo"),
        }
    }
}
