use super::RevisionStyle;
use super::StatusEmitter;
use super::Summary;
use super::TestStatus;
use crate::TestOk;
use crate::test_result::TestResult;

use std::boxed::Box;
use std::fmt::Debug;
use std::path::{Path, PathBuf};

use bstr::ByteSlice;

// MAINTENANCE REGION START

// When integrating with a new libtest version, update all emit_xxx functions.

fn emit_suite_end(failed: usize, filtered_out: usize, ignored: usize, passed: usize, status: &String,) {
    // Adapted from test::formatters::json::write_run_finish().
    println!(r#"{{ "type": "suite", "event": "{status}", "passed": {passed}, "failed": {failed}, "ignored": {ignored}, "measured": 0, "filtered_out": {filtered_out} }}"#);
}

fn emit_test_end(name: &String, status: &String, error_output: &String) {
    let escaped_name = serde_json::to_string(name).unwrap();
    let escaped_error_output = if error_output.is_empty() {
        String::new()
    } else {
        format!(r#", "stdout": {}"#, serde_json::to_string(error_output).unwrap())
    };

    // Adapted from test::formatters::json::write_event().
    println!(r#"{{ "type": "test", "event": "{status}", "name": {escaped_name}{escaped_error_output} }}"#);
}

fn emit_test_start(name: &String) {
    let escaped_name = serde_json::to_string(name).unwrap();

    // Adapted from test::formatters::json::write_test_start().
    println!(r#"{{ "type": "test", "event": "started", "name": {escaped_name} }}"#);
}

// MAINTENANCE REGION END

/// A JSON output emitter.
#[derive(Clone)]
pub struct JSON;

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
            String::from("failed")
        } else {
            String::from("ok")
        };

        emit_suite_end(failed, filtered, ignored, succeeded, &status);

        Box::new(())
    }

    /// Invoked the moment we know a test will later be run.
    /// Useful for progress bars and such.
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus + 'static> {
        let name = path.to_str().unwrap().to_string();

        emit_test_start(&name);

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
            String::from("timeout")
        } else {
            match result {
                Ok(TestOk::Ignored) => String::from("ignored"),
                Ok(TestOk::Ok) => String::from("ok"),
                Err(_) => String::from("failed"),
            }
        };
        let error_output = if let Err(errored) = result {
            let command = &errored.command;
            let stderr = errored.stderr.to_str_lossy();
            let stdout = errored.stdout.to_str_lossy();
            format!(r#"---- command\n\n{command}\n\n\---- stdout\n\n\{stdout}\n\n---- stderr\n\n\{stderr}"#)
        } else {
            String::new()
        };

        emit_test_end(&self.name, &status, &error_output);
    }

    /// Invoked before each failed test prints its errors along with a drop guard that can
    /// get invoked afterwards.
    fn failed_test<'a>(
        &'a self,
        _cmd: &'a str,
        _stderr: &'a [u8],
        _stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> { Box::new(()) }

    /// Create a copy of this test for a new path.
    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        let status = JSONStatus {
            name: self.name.clone(),
            path: path.to_path_buf(),
            revision: self.revision.clone(),
            style: self.style.clone(),
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