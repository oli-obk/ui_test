use super::RevisionStyle;
use super::StatusEmitter;
use super::Summary;
use super::TestStatus;
use crate::TestOk;
use crate::test_result::TestResult;

use std::boxed::Box;
use std::fmt::{Debug, Display, Formatter};
use std::path::{Path, PathBuf};

use bstr::ByteSlice;

// MAINTENANCE REGION START

// When integrating with a new libtest version, update:
//     * All emit_xxx functions.
//     * Struct EscapedString's fmt function.

fn emit_suite_end(failed: usize, filtered_out: usize, ignored: usize, passed: usize, status: &String,) {
    // Adapted from test::formatters::json::write_run_finish().
    println!(r#"{{ "type": "suite", "event": "{status}", "passed": {passed}, "failed": {failed}, "ignored": {ignored}, "measured": 0, "filtered_out": {filtered_out} }}\n"#);
}

fn emit_suite_start(test_count: usize) {
    // Adapted from test::formatters::json::write_run_start().
    println!(r#"{{ "type": "suite", "event": "started", "test_count": {test_count} }}\n"#);
}

fn emit_test_end(name: &String, status: &String, stdout: &String) {
    let triaged_name = EscapedString(name);
    let triaged_stdout = EscapedString(stdout);

    // Adapted from test::formatters::json::write_event().
    println!(r#"{{ "type": "test", "event": "{status}", "name": "{triaged_name}"{triaged_stdout} }}\n"#);
}

fn emit_test_start(name: &String) {
    let triaged_name = EscapedString(name);

    // Adapted from test::formatters::json::write_test_start().
    println!(r#"{{ "type": "test", "event": "started", "name": "{triaged_name}" }}\n"#);
}

// Adapted from test::formatters::json.
/// A formatting utility used to print strings with characters in need of escaping.
/// Base code taken form `libserialize::json::escape_str`.
struct EscapedString<S: AsRef<str>>(S);

impl<S: AsRef<str>> Display for EscapedString<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        let mut start = 0;

        for (i, byte) in self.0.as_ref().bytes().enumerate() {
            let escaped = match byte {
                b'"' => "\\\"",
                b'\\' => "\\\\",
                b'\x00' => "\\u0000",
                b'\x01' => "\\u0001",
                b'\x02' => "\\u0002",
                b'\x03' => "\\u0003",
                b'\x04' => "\\u0004",
                b'\x05' => "\\u0005",
                b'\x06' => "\\u0006",
                b'\x07' => "\\u0007",
                b'\x08' => "\\b",
                b'\t' => "\\t",
                b'\n' => "\\n",
                b'\x0b' => "\\u000b",
                b'\x0c' => "\\f",
                b'\r' => "\\r",
                b'\x0e' => "\\u000e",
                b'\x0f' => "\\u000f",
                b'\x10' => "\\u0010",
                b'\x11' => "\\u0011",
                b'\x12' => "\\u0012",
                b'\x13' => "\\u0013",
                b'\x14' => "\\u0014",
                b'\x15' => "\\u0015",
                b'\x16' => "\\u0016",
                b'\x17' => "\\u0017",
                b'\x18' => "\\u0018",
                b'\x19' => "\\u0019",
                b'\x1a' => "\\u001a",
                b'\x1b' => "\\u001b",
                b'\x1c' => "\\u001c",
                b'\x1d' => "\\u001d",
                b'\x1e' => "\\u001e",
                b'\x1f' => "\\u001f",
                b'\x7f' => "\\u007f",
                _ => {
                    continue;
                }
            };

            if start < i {
                f.write_str(&self.0.as_ref()[start..i])?;
            }
            f.write_str(escaped)?;

            start = i + 1;
        }

        if start != self.0.as_ref().len() {
            f.write_str(&self.0.as_ref()[start..])?;
        }
        Ok(())
    }
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

        emit_suite_start(failed + ignored + succeeded);
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
        let stdout = if let Err(errored) = result {
            let command = &errored.command;
            let stderr = errored.stderr.to_str_lossy();
            let stdout = errored.stdout.to_str_lossy();
            format!(r#", "stdout": "---- command\n\n{command}\n\n\---- stdout\n\n\{stdout}\n\n---- stderr\n\n\{stderr}""#)
        } else {
            String::new()
        };

        emit_test_end(&self.name, &status, &stdout);
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