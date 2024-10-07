//! Various schemes for reporting messages during testing or after testing is done.

use crate::{test_result::TestResult, Errors};

use std::{
    fmt::Debug,
    panic::RefUnwindSafe,
    path::{Path, PathBuf},
};
pub use text::*;
pub mod debug;
mod text;
#[cfg(feature = "gha")]
pub use gha::*;

/// A generic way to handle the output of this crate.
pub trait StatusEmitter: Sync + RefUnwindSafe {
    /// Invoked the moment we know a test will later be run.
    /// Useful for progress bars and such.
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus + 'static>;

    /// Create a report about the entire test run at the end.
    #[allow(clippy::type_complexity)]
    fn finalize(
        &self,
        failed: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary>;
}

/// Some configuration options for revisions
#[derive(Debug, Clone, Copy)]
pub enum RevisionStyle {
    /// Things like dependencies or aux files building are not really nested
    /// below the build, but it is waiting on it.
    Separate,
    /// Always show them, even if rendering to a file
    Show,
}

/// Information about a specific test run.
pub trait TestStatus: Send + Sync + RefUnwindSafe {
    /// Create a copy of this test for a new revision.
    fn for_revision(&self, revision: &str, style: RevisionStyle) -> Box<dyn TestStatus>;

    /// Create a copy of this test for a new path.
    fn for_path(&self, path: &Path) -> Box<dyn TestStatus>;

    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// gets invoked afterwards.
    fn failed_test<'a>(
        &'a self,
        cmd: &'a str,
        stderr: &'a [u8],
        stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a>;

    /// A test has finished, handle the result immediately.
    fn done(&self, _result: &TestResult, _aborted: bool) {}

    /// The path of the test file.
    fn path(&self) -> &Path;

    /// The revision, usually an empty string.
    fn revision(&self) -> &str;
}

/// Report a summary at the end of a test run.
pub trait Summary {
    /// A test has finished, handle the result.
    fn test_failure(&mut self, _status: &dyn TestStatus, _errors: &Errors) {}
}

/// Report no summary
impl Summary for () {}

/// Emit nothing
impl StatusEmitter for () {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        Box::new(SilentStatus {
            path,
            revision: String::new(),
        })
    }

    fn finalize(
        &self,
        _failed: usize,
        _succeeded: usize,
        _ignored: usize,
        _filtered: usize,
        _aborted: bool,
    ) -> Box<dyn Summary> {
        Box::new(())
    }
}

/// When you need a dummy value that doesn't actually print anything
pub struct SilentStatus {
    /// Forwarded to `TestStatus::revision`
    pub revision: String,
    /// Forwarded to `TestStatus::path`
    pub path: PathBuf,
}

impl TestStatus for SilentStatus {
    fn for_revision(&self, revision: &str, _style: RevisionStyle) -> Box<dyn TestStatus> {
        Box::new(SilentStatus {
            revision: revision.into(),
            path: self.path.clone(),
        })
    }

    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        Box::new(SilentStatus {
            revision: self.revision.clone(),
            path: path.to_path_buf(),
        })
    }

    fn failed_test<'a>(
        &'a self,
        _cmd: &'a str,
        _stderr: &'a [u8],
        _stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        Box::new(())
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn revision(&self) -> &str {
        &self.revision
    }
}

#[cfg(feature = "gha")]
mod gha {
    use crate::{diagnostics::Message, display, Error, Errors};

    use crate::github_actions;
    use bstr::ByteSlice;
    use spanned::{Span, Spanned};
    use std::{
        fmt::{Debug, Write as _},
        io::Write as _,
        num::NonZeroUsize,
        path::{Path, PathBuf},
    };

    use super::{RevisionStyle, StatusEmitter, Summary, TestStatus};
    fn gha_error(error: &Error, test_path: &str, revision: &str) {
        let file = Spanned::read_from_file(test_path).unwrap();
        let line = |span: &Span| {
            let line = file
                .lines()
                .position(|line| line.span.bytes.contains(&span.bytes.start))
                .unwrap();
            NonZeroUsize::new(line + 1).unwrap()
        };
        match error {
            Error::ExitStatus {
                status,
                expected,
                reason,
            } => {
                let mut err = github_actions::error(
                    test_path,
                    format!("test{revision} got {status}, but expected {expected}"),
                );
                err.write_str(reason).unwrap();
            }
            Error::Command { kind, status } => {
                github_actions::error(test_path, format!("{kind}{revision} failed with {status}"));
            }
            Error::PatternNotFound { pattern, .. } => {
                github_actions::error(test_path, format!("Pattern not found{revision}"))
                    .line(line(&pattern.span));
            }
            Error::CodeNotFound { code, .. } => {
                github_actions::error(test_path, format!("Diagnostic code not found{revision}"))
                    .line(line(&code.span));
            }
            Error::NoPatternsFound => {
                github_actions::error(
                    test_path,
                    format!("expexted error patterns, but found none{revision}"),
                );
            }
            Error::PatternFoundInPassTest { .. } => {
                github_actions::error(
                    test_path,
                    format!("error pattern found in pass test{revision}"),
                );
            }
            Error::OutputDiffers {
                path: output_path,
                actual,
                expected,
                bless_command,
            } => {
                if expected.is_empty() {
                    let mut err = github_actions::error(
                        test_path,
                        "test generated output, but there was no output file",
                    );
                    if let Some(bless_command) = bless_command {
                        writeln!(
                            err,
                            "you likely need to bless the tests with `{bless_command}`"
                        )
                        .unwrap();
                    }
                    return;
                }

                let mut line = 1;
                for r in
                    prettydiff::diff_lines(expected.to_str().unwrap(), actual.to_str().unwrap())
                        .diff()
                {
                    use prettydiff::basic::DiffOp::*;
                    match r {
                        Equal(s) => {
                            line += s.len();
                            continue;
                        }
                        Replace(l, r) => {
                            let mut err = github_actions::error(
                                display(output_path),
                                "actual output differs from expected",
                            )
                            .line(NonZeroUsize::new(line + 1).unwrap());
                            writeln!(err, "this line was expected to be `{}`", r[0]).unwrap();
                            line += l.len();
                        }
                        Remove(l) => {
                            let mut err = github_actions::error(
                                display(output_path),
                                "extraneous lines in output",
                            )
                            .line(NonZeroUsize::new(line + 1).unwrap());
                            writeln!(
                                err,
                                "remove this line and possibly later ones by blessing the test"
                            )
                            .unwrap();
                            line += l.len();
                        }
                        Insert(r) => {
                            let mut err = github_actions::error(
                                display(output_path),
                                "missing line in output",
                            )
                            .line(NonZeroUsize::new(line + 1).unwrap());
                            writeln!(err, "bless the test to create a line containing `{}`", r[0])
                                .unwrap();
                            // Do not count these lines, they don't exist in the original file and
                            // would thus mess up the line number.
                        }
                    }
                }
            }
            Error::ErrorsWithoutPattern { path, msgs } => {
                if let Some((path, line)) = path.as_ref() {
                    let path = display(path);
                    let mut err =
                        github_actions::error(path, format!("Unmatched diagnostics{revision}"))
                            .line(*line);
                    for Message {
                        level,
                        message,
                        line: _,
                        span: _,
                        code: _,
                    } in msgs
                    {
                        writeln!(err, "{level:?}: {message}").unwrap();
                    }
                } else {
                    let mut err = github_actions::error(
                        test_path,
                        format!("Unmatched diagnostics outside the testfile{revision}"),
                    );
                    for Message {
                        level,
                        message,
                        line: _,
                        span: _,
                        code: _,
                    } in msgs
                    {
                        writeln!(err, "{level:?}: {message}").unwrap();
                    }
                }
            }
            Error::InvalidComment { msg, span } => {
                let mut err = github_actions::error(test_path, format!("Could not parse comment"))
                    .line(line(span));
                writeln!(err, "{msg}").unwrap();
            }
            Error::MultipleRevisionsWithResults { kind, lines } => {
                github_actions::error(test_path, format!("multiple {kind} found"))
                    .line(line(&lines[0]));
            }
            Error::Bug(_) => {}
            Error::Aux {
                path: aux_path,
                errors,
            } => {
                github_actions::error(test_path, format!("Aux build failed"))
                    .line(line(&aux_path.span));
                for error in errors {
                    gha_error(error, &display(aux_path), "")
                }
            }
            Error::Rustfix(error) => {
                github_actions::error(
                    test_path,
                    format!("failed to apply suggestions with rustfix: {error}"),
                );
            }
            Error::ConfigError(msg) => {
                github_actions::error(test_path, msg.clone());
            }
        }
    }

    /// Emits Github Actions Workspace commands to show the failures directly in the github diff view.
    /// If the const generic `GROUP` boolean is `true`, also emit `::group` commands.
    pub struct Gha<const GROUP: bool> {
        /// Show a specific name for the final summary.
        pub name: String,
    }

    #[derive(Clone)]
    struct PathAndRev<const GROUP: bool> {
        path: PathBuf,
        revision: String,
    }

    impl<const GROUP: bool> TestStatus for PathAndRev<GROUP> {
        fn path(&self) -> &Path {
            &self.path
        }

        fn for_revision(&self, revision: &str, _style: RevisionStyle) -> Box<dyn TestStatus> {
            Box::new(Self {
                path: self.path.clone(),
                revision: revision.to_owned(),
            })
        }

        fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
            Box::new(Self {
                path: path.to_path_buf(),
                revision: self.revision.clone(),
            })
        }

        fn failed_test(&self, _cmd: &str, _stderr: &[u8], _stdout: &[u8]) -> Box<dyn Debug> {
            if GROUP {
                Box::new(github_actions::group(format_args!(
                    "{}:{}",
                    display(&self.path),
                    self.revision
                )))
            } else {
                Box::new(())
            }
        }

        fn revision(&self) -> &str {
            &self.revision
        }
    }

    impl<const GROUP: bool> StatusEmitter for Gha<GROUP> {
        fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
            Box::new(PathAndRev::<GROUP> {
                path,
                revision: String::new(),
            })
        }

        fn finalize(
            &self,
            _failures: usize,
            succeeded: usize,
            ignored: usize,
            filtered: usize,
            // Can't aborted on gha
            _aborted: bool,
        ) -> Box<dyn Summary> {
            struct Summarizer<const GROUP: bool> {
                failures: Vec<String>,
                succeeded: usize,
                ignored: usize,
                filtered: usize,
                name: String,
            }

            impl<const GROUP: bool> Summary for Summarizer<GROUP> {
                fn test_failure(&mut self, status: &dyn TestStatus, errors: &Errors) {
                    let revision = if status.revision().is_empty() {
                        "".to_string()
                    } else {
                        format!(" (revision: {})", status.revision())
                    };
                    for error in errors {
                        gha_error(error, &display(status.path()), &revision);
                    }
                    self.failures
                        .push(format!("{}{revision}", display(status.path())));
                }
            }
            impl<const GROUP: bool> Drop for Summarizer<GROUP> {
                fn drop(&mut self) {
                    if let Some(mut file) = github_actions::summary() {
                        writeln!(file, "### {}", self.name).unwrap();
                        for line in &self.failures {
                            writeln!(file, "* {line}").unwrap();
                        }
                        writeln!(file).unwrap();
                        writeln!(file, "| failed | passed | ignored | filtered out |").unwrap();
                        writeln!(file, "| --- | --- | --- | --- |").unwrap();
                        writeln!(
                            file,
                            "| {} | {} | {} | {} |",
                            self.failures.len(),
                            self.succeeded,
                            self.ignored,
                            self.filtered,
                        )
                        .unwrap();
                    }
                }
            }

            Box::new(Summarizer::<GROUP> {
                failures: vec![],
                succeeded,
                ignored,
                filtered,
                name: self.name.clone(),
            })
        }
    }
}

impl<T: TestStatus, U: TestStatus> TestStatus for (T, U) {
    fn done(&self, result: &TestResult, aborted: bool) {
        self.0.done(result, aborted);
        self.1.done(result, aborted);
    }

    fn failed_test<'a>(
        &'a self,
        cmd: &'a str,
        stderr: &'a [u8],
        stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        Box::new((
            self.0.failed_test(cmd, stderr, stdout),
            self.1.failed_test(cmd, stderr, stdout),
        ))
    }

    fn path(&self) -> &Path {
        let path = self.0.path();
        assert_eq!(path, self.1.path());
        path
    }

    fn revision(&self) -> &str {
        let rev = self.0.revision();
        assert_eq!(rev, self.1.revision());
        rev
    }

    fn for_revision(&self, revision: &str, style: RevisionStyle) -> Box<dyn TestStatus> {
        Box::new((
            self.0.for_revision(revision, style),
            self.1.for_revision(revision, style),
        ))
    }

    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        Box::new((self.0.for_path(path), self.1.for_path(path)))
    }
}

impl<T: StatusEmitter, U: StatusEmitter> StatusEmitter for (T, U) {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        Box::new((
            self.0.register_test(path.clone()),
            self.1.register_test(path),
        ))
    }

    fn finalize(
        &self,
        failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary> {
        Box::new((
            self.1
                .finalize(failures, succeeded, ignored, filtered, aborted),
            self.0
                .finalize(failures, succeeded, ignored, filtered, aborted),
        ))
    }
}

/// Forwards directly to `T`, exists only so that tuples can be used with `cfg` to filter
/// out individual fields
impl<T: StatusEmitter> StatusEmitter for (T,) {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        self.0.register_test(path.clone())
    }

    fn finalize(
        &self,
        failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary> {
        self.0
            .finalize(failures, succeeded, ignored, filtered, aborted)
    }
}

impl<T: TestStatus + ?Sized> TestStatus for Box<T> {
    fn done(&self, result: &TestResult, aborted: bool) {
        (**self).done(result, aborted);
    }

    fn path(&self) -> &Path {
        (**self).path()
    }

    fn revision(&self) -> &str {
        (**self).revision()
    }

    fn for_revision(&self, revision: &str, style: RevisionStyle) -> Box<dyn TestStatus> {
        (**self).for_revision(revision, style)
    }

    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        (**self).for_path(path)
    }

    fn failed_test<'a>(
        &'a self,
        cmd: &'a str,
        stderr: &'a [u8],
        stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        (**self).failed_test(cmd, stderr, stdout)
    }
}

impl<T: StatusEmitter + ?Sized> StatusEmitter for Box<T> {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        (**self).register_test(path)
    }

    fn finalize(
        &self,
        failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary> {
        (**self).finalize(failures, succeeded, ignored, filtered, aborted)
    }
}

impl Summary for (Box<dyn Summary>, Box<dyn Summary>) {
    fn test_failure(&mut self, status: &dyn TestStatus, errors: &Errors) {
        self.0.test_failure(status, errors);
        self.1.test_failure(status, errors);
    }
}
