use super::RevisionStyle;
use super::StatusEmitter;
use super::Summary;
use super::TestStatus;
use crate::test_result::TestResult;
use crate::TestOk;

use std::boxed::Box;
use std::fmt::Debug;
use std::path::{Path, PathBuf};

use bstr::ByteSlice;

// MAINTENANCE REGION START

// When integrating with a new libtest version, update all emit_xxx functions.

fn emit_suite_end(failed: usize, filtered_out: usize, ignored: usize, passed: usize, status: &str) {
    // Adapted from test::formatters::json::write_run_finish().
    println!(
        r#"{{ "type": "suite", "event": "{status}", "passed": {passed}, "failed": {failed}, "ignored": {ignored}, "measured": 0, "filtered_out": {filtered_out} }}"#
    );
}

fn emit_suite_start() {
    // Adapted from test::formatters::json::write_run_start().
    println!(r#"{{ "type": "suite", "event": "started" }}"#);
}

fn emit_test_end(name: &String, revision: &String, path: &Path, status: &str, diags: &str) {
    let displayed_path = path.display();
    let stdout = if diags.is_empty() {
        String::new()
    } else {
        let triaged_diags = serde_json::to_string(diags).unwrap();
        format!(r#", "stdout": {triaged_diags}"#)
    };

    // Adapted from test::formatters::json::write_event().
    println!(
        r#"{{ "type": "test", "event": "{status}", "name": "{name} ({revision}) - {displayed_path}"{stdout} }}"#
    );
}

fn emit_test_start(name: &String, revision: &String, path: &Path) {
    let displayed_path = path.display();

    // Adapted from test::formatters::json::write_test_start().
    println!(
        r#"{{ "type": "test", "event": "started", "name": "{name} ({revision}) - {displayed_path}" }}"#
    );
}

// MAINTENANCE REGION END

/// A JSON output emitter.
#[derive(Clone)]
pub struct JSON {}

impl JSON {
    /// Create a new instance of a JSON output emitter.
    pub fn new() -> Self {
        emit_suite_start();

        JSON {}
    }
}

impl Default for JSON {
    fn default() -> Self {
        Self::new()
    }
}

impl StatusEmitter for JSON {
    /// Create a report about the entire test run at the end.
    fn finalize(
        &self,
        failed: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary> {
        let status = if aborted || failed > 0 {
            "failed"
        } else {
            "ok"
        };

        emit_suite_end(failed, filtered, ignored, succeeded, status);

        Box::new(())
    }

    /// Invoked the moment we know a test will later be run.
    /// Useful for progress bars and such.
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus + 'static> {
        let name = path.to_str().unwrap().to_string();
        let revision = String::new();

        emit_test_start(&name, &revision, &path);

        Box::new(JSONStatus {
            name,
            path,
            revision: String::new(),
            style: RevisionStyle::Show,
        })
    }
}

/// Information about a specific test run.
pub struct JSONStatus {
    name: String,
    path: PathBuf,
    revision: String,
    style: RevisionStyle,
}

impl TestStatus for JSONStatus {
    /// A test has finished, handle the result immediately.
    fn done(&self, result: &TestResult, aborted: bool) {
        let status = if aborted {
            "timeout"
        } else {
            match result {
                Ok(TestOk::Ignored) => "ignored",
                Ok(TestOk::Ok) => "ok",
                Err(_) => "failed",
            }
        };
        let diags = if let Err(errored) = result {
            let command = errored.command.as_str();
            let stdout = errored.stderr.to_str_lossy();
            let stderr = errored.stdout.to_str_lossy();

            format!(r#"command: <{command}> stdout: <{stdout}> stderr: <{stderr}>"#)
        } else {
            String::new()
        };

        emit_test_end(&self.name, &self.revision, self.path(), status, &diags);
    }

    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// get invoked afterwards.
    fn failed_test<'a>(
        &'a self,
        _cmd: &'a str,
        _stderr: &'a [u8],
        _stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        Box::new(())
    }

    /// Create a copy of this test for a new path.
    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        let status = JSONStatus {
            name: self.name.clone(),
            path: path.to_path_buf(),
            revision: self.revision.clone(),
            style: self.style,
        };
        Box::new(status)
    }

    /// Create a copy of this test for a new revision.
    fn for_revision(&self, revision: &str, style: RevisionStyle) -> Box<dyn TestStatus> {
        let status = JSONStatus {
            name: self.name.clone(),
            path: self.path.clone(),
            revision: revision.to_owned(),
            style,
        };
        Box::new(status)
    }

    /// The path of the test file.
    fn path(&self) -> &Path {
        &self.path
    }

    /// The revision, usually an empty string.
    fn revision(&self) -> &str {
        &self.revision
    }
}
