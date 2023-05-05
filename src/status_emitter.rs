//! Variaous schemes for reporting messages during testing or after testing is done.

use colored::Colorize;

use crate::github_actions;
use std::{fmt::Debug, io::Write, path::Path, process::Command};

/// A generic way to handle the output of this crate.
pub trait StatusEmitter {
    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// gets invoked afterwards.
    fn failed_test<'a>(
        &'a self,
        revision: &'a str,
        path: &'a Path,
        cmd: &'a Command,
        stderr: &'a [u8],
    ) -> Box<dyn Debug + 'a>;
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
}
