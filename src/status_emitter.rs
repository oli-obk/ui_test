//! Variaous schemes for reporting messages during testing or after testing is done.

use colored::Colorize;

use crate::{github_actions, Config, TestResult};
use std::{fmt::Debug, io::Write, path::Path, process::Command};

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

    /// Start a test run and return a handle for reporting individual tests' results
    fn run_tests(&self, config: &Config) -> Box<dyn DuringTestRun>;
}

/// Report information during test runs.
pub trait DuringTestRun {
    /// A test has finished, handle the result.
    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult);
}

/// A human readable output emitter.
pub struct Text;
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

    fn run_tests(&self, config: &Config) -> Box<dyn DuringTestRun> {
        if config.quiet {
            Box::new(Quiet { n: 0 })
        } else {
            Box::new(Text)
        }
    }
}

impl DuringTestRun for Text {
    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult) {
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

struct Quiet {
    n: usize,
}

impl DuringTestRun for Quiet {
    fn test_result(&mut self, _path: &Path, _revision: &str, result: &TestResult) {
        // Humans start counting at 1
        self.n += 1;
        match result {
            TestResult::Ok => eprint!("{}", ".".green()),
            TestResult::Errored { .. } => eprint!("{}", "F".red().bold()),
            TestResult::Ignored => eprint!("{}", "i".yellow()),
            TestResult::Filtered => {}
        }
        if self.n % 100 == 0 {
            eprintln!(" {}", self.n);
        }
    }
}

/// Emits Github Actions Workspace commands to show the failures directly in the github diff view.
pub struct Gha;
impl StatusEmitter for Gha {
    fn failed_test(
        &self,
        revision: &str,
        path: &Path,
        _cmd: &Command,
        _stderr: &[u8],
    ) -> Box<dyn Debug> {
        Box::new(github_actions::group(format_args!(
            "{}:{revision}",
            path.display()
        )))
    }

    fn run_tests(&self, _config: &Config) -> Box<dyn DuringTestRun> {
        Box::new(Gha)
    }
}

impl DuringTestRun for Gha {
    fn test_result(&mut self, _path: &Path, _revision: &str, _result: &TestResult) {}
}

/// Prints a human readable message as well as a github action workflow command where applicable.
pub struct TextAndGha;
impl StatusEmitter for TextAndGha {
    fn failed_test<'a>(
        &'a self,
        revision: &'a str,
        path: &'a Path,
        cmd: &'a Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        Box::new((
            Gha.failed_test(revision, path, cmd, stderr),
            Text.failed_test(revision, path, cmd, stderr),
        ))
    }

    fn run_tests(&self, _config: &Config) -> Box<dyn DuringTestRun> {
        Box::new(TextAndGha)
    }
}

impl DuringTestRun for TextAndGha {
    fn test_result(&mut self, path: &Path, revision: &str, result: &TestResult) {
        Text.test_result(path, revision, result);
        Gha.test_result(path, revision, result);
    }
}
