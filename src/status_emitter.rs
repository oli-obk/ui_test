//! Variaous schemes for reporting messages during testing or after testing is done.

use bstr::ByteSlice;
use colored::Colorize;

use crate::{github_actions, parser::Pattern, rustc_stderr::Message, Error, Errors, TestResult};
use std::{
    fmt::{Debug, Write as _},
    io::Write as _,
    path::Path,
    process::Command,
};

/// A generic way to handle the output of this crate.
pub trait StatusEmitter: Sync {
    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// gets invoked afterwards.
    fn failed_test<'a>(
        &'a self,
        revision: &'a str,
        path: &'a Path,
        cmd: &'a Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a>;

    /// A test has finished, handle the result immediately.
    fn test_result(&mut self, _path: &Path, _revision: &str, _result: &TestResult) {}

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

/// Report a summary at the end of a test run.
pub trait Summary {
    /// A test has finished, handle the result.
    fn test_failure(&mut self, _path: &Path, _revision: &str, _errors: &Errors) {}
}

impl Summary for () {}

/// A human readable output emitter.
pub struct Text {
    /// In case of `Some`, the `usize` is the number of tests
    /// that were already executed.
    quiet: Option<usize>,
}

impl Text {
    /// Print one line per test that gets run.
    pub fn verbose() -> Self {
        Self { quiet: None }
    }
    /// Print one `.` per test that gets run.
    pub fn quiet() -> Self {
        Self { quiet: Some(0) }
    }
}

impl StatusEmitter for Text {
    fn failed_test<'a>(
        &self,
        revision: &str,
        path: &Path,
        cmd: &Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        eprintln!();
        let path = path.display().to_string();
        eprint!("{}", path.underline().bold());
        let revision = if revision.is_empty() {
            String::new()
        } else {
            format!(" (revision `{revision}`)")
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

    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult) {
        if let Some(n) = &mut self.quiet {
            // Humans start counting at 1
            *n += 1;
            match result {
                TestResult::Ok => eprint!("{}", ".".green()),
                TestResult::Errored { .. } => eprint!("{}", "F".red().bold()),
                TestResult::Ignored => eprint!("{}", "i".yellow()),
                TestResult::Filtered => {}
            }
            if *n % 100 == 0 {
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
                path.display(),
                if revision.is_empty() {
                    "".into()
                } else {
                    format!(" ({revision})")
                }
            );
            eprintln!("{result}");
        }
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
                fn test_failure(&mut self, path: &Path, revision: &str, errors: &Errors) {
                    for error in errors {
                        print_error(error, &path.display().to_string());
                    }

                    self.failures.push(if revision.is_empty() {
                        format!("    {}", path.display())
                    } else {
                        format!("    {} (revision {revision})", path.display())
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
        Error::PatternNotFound {
            pattern,
            definition_line,
        } => {
            match pattern {
                Pattern::SubString(s) => {
                    eprintln!("substring `{s}` {} in stderr output", "not found".red())
                }
                Pattern::Regex(r) => {
                    eprintln!("`/{r}/` does {} stderr output", "not match".red())
                }
            }
            eprintln!(
                "expected because of pattern here: {}",
                format!("{path}:{definition_line}").bold()
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
        Error::ErrorsWithoutPattern { path: None, msgs } => {
            eprintln!(
                "There were {} unmatched diagnostics that occurred outside the testfile and had no pattern",
                msgs.len(),
            );
            for Message { level, message } in msgs {
                eprintln!("    {level:?}: {message}")
            }
        }
        Error::ErrorsWithoutPattern {
            path: Some((path, line)),
            msgs,
        } => {
            let path = path.display();
            eprintln!(
                "There were {} unmatched diagnostics at {path}:{line}",
                msgs.len(),
            );
            for Message { level, message } in msgs {
                eprintln!("    {level:?}: {message}")
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

fn gha_error(error: &Error, path: &str, revision: &str) {
    match error {
        Error::ExitStatus {
            mode,
            status,
            expected,
        } => {
            github_actions::error(
                path,
                format!("{mode} test{revision} got {status}, but expected {expected}"),
            );
        }
        Error::Command { kind, status } => {
            github_actions::error(path, format!("{kind}{revision} failed with {status}"));
        }
        Error::PatternNotFound {
            pattern: _,
            definition_line,
        } => {
            github_actions::error(path, format!("Pattern not found{revision}"))
                .line(*definition_line);
        }
        Error::NoPatternsFound => {
            github_actions::error(
                path,
                format!("no error patterns found in fail test{revision}"),
            );
        }
        Error::PatternFoundInPassTest => {
            github_actions::error(path, format!("error pattern found in pass test{revision}"));
        }
        Error::OutputDiffers {
            path: output_path,
            actual,
            expected,
            bless_command: _,
        } => {
            let mut err = github_actions::error(
                if expected.is_empty() {
                    path.to_owned()
                } else {
                    output_path.display().to_string()
                },
                "actual output differs from expected",
            );
            writeln!(err, "```diff").unwrap();
            let mut seen_diff_line = Some(0);
            for r in ::diff::lines(expected.to_str().unwrap(), actual.to_str().unwrap()) {
                if let Some(line) = &mut seen_diff_line {
                    *line += 1;
                }
                let mut seen_diff = || {
                    if let Some(line) = seen_diff_line.take() {
                        writeln!(err, "{line} unchanged lines skipped").unwrap();
                    }
                };
                match r {
                    ::diff::Result::Both(l, r) => {
                        if l != r {
                            seen_diff();
                            writeln!(err, "-{l}").unwrap();
                            writeln!(err, "+{r}").unwrap();
                        } else if seen_diff_line.is_none() {
                            writeln!(err, " {l}").unwrap()
                        }
                    }
                    ::diff::Result::Left(l) => {
                        seen_diff();
                        writeln!(err, "-{l}").unwrap();
                    }
                    ::diff::Result::Right(r) => {
                        seen_diff();
                        writeln!(err, "+{r}").unwrap();
                    }
                }
            }
            writeln!(err, "```").unwrap();
        }
        Error::ErrorsWithoutPattern { path: None, msgs } => {
            let mut err = github_actions::error(
                path,
                format!("Unmatched diagnostics outside the testfile{revision}"),
            );
            for Message { level, message } in msgs {
                writeln!(err, "{level:?}: {message}").unwrap();
            }
        }
        Error::ErrorsWithoutPattern {
            path: Some((path, line)),
            msgs,
        } => {
            let path = path.display();
            let mut err = github_actions::error(&path, format!("Unmatched diagnostics{revision}"))
                .line(*line);
            for Message { level, message } in msgs {
                writeln!(err, "{level:?}: {message}").unwrap();
            }
        }
        Error::InvalidComment { msg, line } => {
            let mut err =
                github_actions::error(path, format!("Could not parse comment")).line(*line);
            writeln!(err, "{msg}").unwrap();
        }
        Error::Bug(_) => {}
        Error::Aux {
            path: aux_path,
            errors,
            line,
        } => {
            github_actions::error(path, format!("Aux build failed")).line(*line);
            for error in errors {
                gha_error(error, &aux_path.display().to_string(), "")
            }
        }
        Error::Rustfix(error) => {
            github_actions::error(
                path,
                format!("failed to apply suggestions with rustfix: {error}"),
            );
        }
    }
    eprintln!();
}

/// Emits Github Actions Workspace commands to show the failures directly in the github diff view.
/// If the const generic `GROUP` boolean is `true`, also emit `::group` commands.
pub struct Gha<const GROUP: bool> {
    /// Show a specific name for the final summary.
    pub name: String,
}

impl<const GROUP: bool> StatusEmitter for Gha<GROUP> {
    fn failed_test(
        &self,
        revision: &str,
        path: &Path,
        _cmd: &Command,
        _stderr: &[u8],
    ) -> Box<dyn Debug> {
        if GROUP {
            Box::new(github_actions::group(format_args!(
                "{}:{revision}",
                path.display()
            )))
        } else {
            Box::new(())
        }
    }

    fn test_result(&mut self, _path: &Path, _revision: &str, _result: &TestResult) {}

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
            fn test_failure(&mut self, path: &Path, revision: &str, errors: &Errors) {
                let revision = if revision.is_empty() {
                    "".to_string()
                } else {
                    format!(" (revision: {revision})")
                };
                for error in errors {
                    gha_error(error, &path.display().to_string(), &revision);
                }
                self.failures.push(format!("{}{revision}", path.display()));
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

impl<T: StatusEmitter, U: StatusEmitter> StatusEmitter for (T, U) {
    fn failed_test<'a>(
        &'a self,
        revision: &'a str,
        path: &'a Path,
        cmd: &'a Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        Box::new((
            self.0.failed_test(revision, path, cmd, stderr),
            self.1.failed_test(revision, path, cmd, stderr),
        ))
    }

    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult) {
        self.0.test_result(path, revision, result);
        self.1.test_result(path, revision, result);
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

impl<T: StatusEmitter + ?Sized> StatusEmitter for Box<T> {
    fn failed_test<'a>(
        &'a self,
        revision: &'a str,
        path: &'a Path,
        cmd: &'a Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        (**self).failed_test(revision, path, cmd, stderr)
    }

    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult) {
        (**self).test_result(path, revision, result);
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
    fn test_failure(&mut self, path: &Path, revision: &str, errors: &Errors) {
        self.0.test_failure(path, revision, errors);
        self.1.test_failure(path, revision, errors);
    }
}
