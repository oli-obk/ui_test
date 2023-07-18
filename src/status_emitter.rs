//! Variaous schemes for reporting messages during testing or after testing is done.

use bstr::ByteSlice;
use colored::Colorize;

use crate::{github_actions, parser::Pattern, rustc_stderr::Message, Error, Errors, TestResult};
use std::{
    fmt::{Debug, Write as _},
    io::Write as _,
    num::NonZeroUsize,
    panic::RefUnwindSafe,
    path::{Path, PathBuf},
    process::Command,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

/// A generic way to handle the output of this crate.
pub trait StatusEmitter: Sync {
    /// Invoked the moment we know a test will later be run.
    /// Useful for progress bars and such.
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus>;

    /// Create a report about the entire test run at the end.
    #[allow(clippy::type_complexity)]
    fn finalize(
        &self,
        failed: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
    ) -> Box<dyn Summary>;
}

/// Information about a specific test run.
pub trait TestStatus: Send + Sync + RefUnwindSafe {
    /// Create a copy of this test for a new revision.
    fn for_revision(&self, revision: &str) -> Box<dyn TestStatus>;

    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// gets invoked afterwards.
    fn failed_test<'a>(&'a self, cmd: &'a Command, stderr: &'a [u8]) -> Box<dyn Debug + 'a>;

    /// A test has finished, handle the result immediately.
    fn done(&self, _result: &TestResult) {}

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

impl Summary for () {}

/// A human readable output emitter.
pub struct Text {
    /// In case of `Some`, the `usize` is the number of tests
    /// that were already executed.
    quiet: Option<Arc<AtomicUsize>>,
}

impl Text {
    /// Print one line per test that gets run.
    pub fn verbose() -> Self {
        Self { quiet: None }
    }
    /// Print one `.` per test that gets run.
    pub fn quiet() -> Self {
        Self {
            quiet: Some(Arc::new(AtomicUsize::new(0))),
        }
    }
}

struct TextTest {
    quiet: Option<Arc<AtomicUsize>>,
    path: PathBuf,
    revision: String,
}

impl TestStatus for TextTest {
    fn done(&self, result: &TestResult) {
        if let Some(n) = &self.quiet {
            // Humans start counting at 1
            let n = n.fetch_add(1, Ordering::Release);
            match result {
                TestResult::Ok => eprint!("{}", ".".green()),
                TestResult::Errored { .. } => eprint!("{}", "F".red().bold()),
                TestResult::Ignored => eprint!("{}", "i".yellow()),
                TestResult::Filtered => {}
            }
            if (n + 1) % 100 == 0 {
                eprintln!(" {}", n);
            }
        } else {
            let result = match result {
                TestResult::Ok => "ok".green(),
                TestResult::Errored { .. } => "FAILED".red().bold(),
                TestResult::Ignored => "ignored (in-test comment)".yellow(),
                TestResult::Filtered => return,
            };
            eprint!(
                "{}{} ... ",
                self.path.display(),
                if self.revision.is_empty() {
                    "".into()
                } else {
                    format!(" ({})", self.revision)
                }
            );
            eprintln!("{result}");
        }
    }

    fn failed_test<'a>(&self, cmd: &Command, stderr: &'a [u8]) -> Box<dyn Debug + 'a> {
        eprintln!();
        let path = self.path.display().to_string();
        eprint!("{}", path.underline().bold());
        let revision = if self.revision.is_empty() {
            String::new()
        } else {
            format!(" (revision `{}`)", self.revision)
        };
        eprint!("{revision}");
        eprint!(" {}", "FAILED:".red().bold());
        eprintln!();
        eprintln!("command: {cmd:?}");
        eprintln!();

        #[derive(Debug)]
        struct Guard<'a>(&'a [u8]);
        impl<'a> Drop for Guard<'a> {
            fn drop(&mut self) {
                eprintln!("full stderr:");
                std::io::stderr().write_all(self.0).unwrap();
                eprintln!();
                eprintln!();
            }
        }
        Box::new(Guard(stderr))
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn for_revision(&self, revision: &str) -> Box<dyn TestStatus> {
        assert_eq!(self.revision, "");
        Box::new(Self {
            quiet: self.quiet.clone(),
            path: self.path.clone(),
            revision: revision.to_owned(),
        })
    }

    fn revision(&self) -> &str {
        &self.revision
    }
}

impl StatusEmitter for Text {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        Box::new(TextTest {
            quiet: self.quiet.clone(),
            path,
            revision: String::new(),
        })
    }

    fn finalize(
        &self,
        failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
    ) -> Box<dyn Summary> {
        // Print all errors in a single thread to show reliable output
        if failures == 0 {
            eprintln!();
            eprintln!(
                "test result: {}. {} tests passed, {} ignored, {} filtered out",
                "ok".green(),
                succeeded.to_string().green(),
                ignored.to_string().yellow(),
                filtered.to_string().yellow(),
            );
            eprintln!();
            Box::new(())
        } else {
            struct Summarizer {
                failures: Vec<String>,
                succeeded: usize,
                ignored: usize,
                filtered: usize,
            }

            impl Summary for Summarizer {
                fn test_failure(&mut self, status: &dyn TestStatus, errors: &Errors) {
                    for error in errors {
                        print_error(error, &status.path().display().to_string());
                    }

                    self.failures.push(if status.revision().is_empty() {
                        format!("    {}", status.path().display())
                    } else {
                        format!(
                            "    {} (revision {})",
                            status.path().display(),
                            status.revision()
                        )
                    });
                }
            }

            impl Drop for Summarizer {
                fn drop(&mut self) {
                    eprintln!("{}", "FAILURES:".red().underline().bold());
                    for line in &self.failures {
                        eprintln!("{line}");
                    }
                    eprintln!();
                    eprintln!(
                        "test result: {}. {} tests failed, {} tests passed, {} ignored, {} filtered out",
                        "FAIL".red(),
                        self.failures.len().to_string().red().bold(),
                        self.succeeded.to_string().green(),
                        self.ignored.to_string().yellow(),
                        self.filtered.to_string().yellow(),
                    );
                }
            }
            Box::new(Summarizer {
                failures: vec![],
                succeeded,
                ignored,
                filtered,
            })
        }
    }
}

fn print_error(error: &Error, path: &str) {
    match error {
        Error::ExitStatus {
            mode,
            status,
            expected,
        } => {
            eprintln!("{mode} test got {status}, but expected {expected}")
        }
        Error::Command { kind, status } => {
            eprintln!("{kind} failed with {status}");
        }
        Error::PatternNotFound(pattern) => {
            match &**pattern {
                Pattern::SubString(s) => {
                    eprintln!("substring `{s}` {} in stderr output", "not found".red())
                }
                Pattern::Regex(r) => {
                    eprintln!("`/{r}/` does {} stderr output", "not match".red())
                }
            }
            eprintln!(
                "expected because of pattern here: {}",
                format!("{path}:{}", pattern.line()).bold()
            );
        }
        Error::NoPatternsFound => {
            eprintln!("{}", "no error patterns found in fail test".red());
        }
        Error::PatternFoundInPassTest => {
            eprintln!("{}", "error pattern found in pass test".red())
        }
        Error::OutputDiffers {
            path: output_path,
            actual,
            expected,
            bless_command,
        } => {
            eprintln!("{}", "actual output differed from expected".underline());
            eprintln!(
                "Execute `{}` to update `{}` to the actual output",
                bless_command,
                output_path.display()
            );
            eprintln!("{}", format!("--- {}", output_path.display()).red());
            eprintln!("{}", "+++ <stderr output>".green());
            crate::diff::print_diff(expected, actual);
        }
        Error::ErrorsWithoutPattern { path, msgs } => {
            if let Some(path) = path.as_ref() {
                let line = path.line();
                let path = path.display();
                eprintln!(
                    "There were {} unmatched diagnostics at {path}:{line}",
                    msgs.len(),
                );
                for Message { level, message } in msgs {
                    eprintln!("    {level:?}: {message}")
                }
            } else {
                eprintln!(
                    "There were {} unmatched diagnostics that occurred outside the testfile and had no pattern",
                    msgs.len(),
                );
                for Message { level, message } in msgs {
                    eprintln!("    {level:?}: {message}")
                }
            }
        }
        Error::InvalidComment { msg, line } => {
            eprintln!("Could not parse comment in {path}:{line} because\n{msg}",)
        }
        Error::Bug(msg) => {
            eprintln!("A bug in `ui_test` occurred: {msg}");
        }
        Error::Aux {
            path: aux_path,
            errors,
            line,
        } => {
            eprintln!("Aux build from {path}:{line} failed");
            for error in errors {
                print_error(error, &aux_path.display().to_string());
            }
        }
        Error::Rustfix(error) => {
            eprintln!("failed to apply suggestions for {path} with rustfix: {error}");
            eprintln!("Add //@no-rustfix to the test file to ignore rustfix suggestions");
        }
    }
    eprintln!();
}

fn gha_error(error: &Error, test_path: &str, revision: &str) {
    match error {
        Error::ExitStatus {
            mode,
            status,
            expected,
        } => {
            github_actions::error(
                test_path,
                format!("{mode} test{revision} got {status}, but expected {expected}"),
            );
        }
        Error::Command { kind, status } => {
            github_actions::error(test_path, format!("{kind}{revision} failed with {status}"));
        }
        Error::PatternNotFound(pattern) => {
            github_actions::error(test_path, format!("Pattern not found{revision}"))
                .line(pattern.line());
        }
        Error::NoPatternsFound => {
            github_actions::error(
                test_path,
                format!("no error patterns found in fail test{revision}"),
            );
        }
        Error::PatternFoundInPassTest => {
            github_actions::error(
                test_path,
                format!("error pattern found in pass test{revision}"),
            );
        }
        Error::OutputDiffers {
            path: output_path,
            actual,
            expected,
            bless_command: _,
        } => {
            if expected.is_empty() {
                let mut err = github_actions::error(
                    test_path,
                    "test generated output, but there was no output file",
                );
                writeln!(err, "you likely need to bless the tests").unwrap();
                return;
            }

            let mut line = 1;
            for r in
                prettydiff::diff_lines(expected.to_str().unwrap(), actual.to_str().unwrap()).diff()
            {
                use prettydiff::basic::DiffOp::*;
                match r {
                    Equal(s) => {
                        line += s.len();
                        continue;
                    }
                    Replace(l, r) => {
                        assert_eq!(r.len(), l.len());
                        let mut err = github_actions::error(
                            output_path.display().to_string(),
                            "actual output differs from expected",
                        )
                        .line(NonZeroUsize::new(line + 1).unwrap());
                        writeln!(err, "this line was expected to be `{}`", r[0]).unwrap();
                        line += l.len();
                    }
                    Remove(l) => {
                        let mut err = github_actions::error(
                            output_path.display().to_string(),
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
                            output_path.display().to_string(),
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
            if let Some(path) = path.as_ref() {
                let line = path.line();
                let path = path.display();
                let mut err =
                    github_actions::error(&path, format!("Unmatched diagnostics{revision}"))
                        .line(line);
                for Message { level, message } in msgs {
                    writeln!(err, "{level:?}: {message}").unwrap();
                }
            } else {
                let mut err = github_actions::error(
                    test_path,
                    format!("Unmatched diagnostics outside the testfile{revision}"),
                );
                for Message { level, message } in msgs {
                    writeln!(err, "{level:?}: {message}").unwrap();
                }
            }
        }
        Error::InvalidComment { msg, line } => {
            let mut err =
                github_actions::error(test_path, format!("Could not parse comment")).line(*line);
            writeln!(err, "{msg}").unwrap();
        }
        Error::Bug(_) => {}
        Error::Aux {
            path: aux_path,
            errors,
            line,
        } => {
            github_actions::error(test_path, format!("Aux build failed")).line(*line);
            for error in errors {
                gha_error(error, &aux_path.display().to_string(), "")
            }
        }
        Error::Rustfix(error) => {
            github_actions::error(
                test_path,
                format!("failed to apply suggestions with rustfix: {error}"),
            );
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

    fn for_revision(&self, revision: &str) -> Box<dyn TestStatus> {
        assert_eq!(self.revision, "");
        Box::new(Self {
            path: self.path.clone(),
            revision: revision.to_owned(),
        })
    }

    fn failed_test(&self, _cmd: &Command, _stderr: &[u8]) -> Box<dyn Debug> {
        if GROUP {
            Box::new(github_actions::group(format_args!(
                "{}:{}",
                self.path.display(),
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
                    gha_error(error, &status.path().display().to_string(), &revision);
                }
                self.failures
                    .push(format!("{}{revision}", status.path().display()));
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

impl<T: TestStatus, U: TestStatus> TestStatus for (T, U) {
    fn done(&self, result: &TestResult) {
        self.0.done(result);
        self.1.done(result);
    }

    fn failed_test<'a>(&'a self, cmd: &'a Command, stderr: &'a [u8]) -> Box<dyn Debug + 'a> {
        Box::new((
            self.0.failed_test(cmd, stderr),
            self.1.failed_test(cmd, stderr),
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

    fn for_revision(&self, revision: &str) -> Box<dyn TestStatus> {
        Box::new((self.0.for_revision(revision), self.1.for_revision(revision)))
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
    ) -> Box<dyn Summary> {
        Box::new((
            self.1.finalize(failures, succeeded, ignored, filtered),
            self.0.finalize(failures, succeeded, ignored, filtered),
        ))
    }
}

impl<T: TestStatus + ?Sized> TestStatus for Box<T> {
    fn done(&self, result: &TestResult) {
        (**self).done(result);
    }

    fn path(&self) -> &Path {
        (**self).path()
    }

    fn revision(&self) -> &str {
        (**self).revision()
    }

    fn for_revision(&self, revision: &str) -> Box<dyn TestStatus> {
        (**self).for_revision(revision)
    }

    fn failed_test<'a>(&'a self, cmd: &'a Command, stderr: &'a [u8]) -> Box<dyn Debug + 'a> {
        (**self).failed_test(cmd, stderr)
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
    ) -> Box<dyn Summary> {
        (**self).finalize(failures, succeeded, ignored, filtered)
    }
}

impl Summary for (Box<dyn Summary>, Box<dyn Summary>) {
    fn test_failure(&mut self, status: &dyn TestStatus, errors: &Errors) {
        self.0.test_failure(status, errors);
        self.1.test_failure(status, errors);
    }
}
