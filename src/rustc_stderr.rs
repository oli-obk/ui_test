use bstr::ByteSlice;
use regex::Regex;
use std::{
    num::NonZeroUsize,
    path::{Path, PathBuf},
};

use crate::diagnostics::{Diagnostics, Message};

#[derive(serde::Deserialize, Debug)]
struct RustcDiagnosticCode {
    code: String,
}

#[derive(serde::Deserialize, Debug)]
struct RustcMessage {
    rendered: Option<String>,
    spans: Vec<RustcSpan>,
    level: String,
    message: String,
    children: Vec<RustcMessage>,
    code: Option<RustcDiagnosticCode>,
}

/// Information about macro expansion.
#[derive(serde::Deserialize, Debug)]
struct Expansion {
    span: RustcSpan,
}

#[derive(serde::Deserialize, Debug)]
struct RustcSpan {
    #[serde(flatten)]
    line_col: Span,
    file_name: PathBuf,
    is_primary: bool,
    expansion: Option<Box<Expansion>>,
}

#[derive(serde::Deserialize, Debug, Copy, Clone)]
struct Span {
    line_start: NonZeroUsize,
    column_start: NonZeroUsize,
    line_end: NonZeroUsize,
    column_end: NonZeroUsize,
}

impl RustcMessage {
    fn line(&self, file: &Path) -> Option<spanned::Span> {
        let span = |primary| self.spans.iter().find_map(|span| span.line(file, primary));
        span(true).or_else(|| span(false))
    }

    /// Put the message and its children into the line-indexed list.
    fn insert_recursive(
        self,
        file: &Path,
        messages: &mut Vec<Vec<Message>>,
        messages_from_unknown_file_or_line: &mut Vec<Message>,
        line: Option<spanned::Span>,
    ) {
        let line = self.line(file).or(line);
        let msg = Message {
            level: self.level.parse().unwrap(),
            message: self.message,
            line_col: line.clone(),
            code: self.code.map(|x| x.code),
        };
        if let Some(line) = line.clone() {
            if messages.len() <= line.line_start.get() {
                messages.resize_with(line.line_start.get() + 1, Vec::new);
            }
            messages[line.line_start.get()].push(msg);
        // All other messages go into the general bin, unless they are specifically of the
        // "aborting due to X previous errors" variety, as we never want to match those. They
        // only count the number of errors and provide no useful information about the tests.
        } else if !(msg.message.starts_with("aborting due to")
            && msg.message.contains("previous error"))
        {
            messages_from_unknown_file_or_line.push(msg);
        }
        for child in self.children {
            child.insert_recursive(
                file,
                messages,
                messages_from_unknown_file_or_line,
                line.clone(),
            )
        }
    }
}

impl RustcSpan {
    /// Returns the most expanded line number *in the given file*, if possible.
    fn line(&self, file: &Path, primary: bool) -> Option<spanned::Span> {
        if let Some(exp) = &self.expansion {
            if let Some(line) = exp.span.line(file, !primary || self.is_primary) {
                return Some(line);
            } else if self.file_name != file {
                return if !primary && self.is_primary {
                    exp.span.line(file, false)
                } else {
                    None
                };
            }
        }
        ((!primary || self.is_primary) && self.file_name == file).then_some(spanned::Span {
            file: self.file_name.clone(),
            line_start: self.line_col.line_start,
            line_end: self.line_col.line_end,
            col_start: self.line_col.column_start,
            col_end: self.line_col.column_end,
        })
    }
}

fn filter_annotations_from_rendered(rendered: &str) -> std::borrow::Cow<'_, str> {
    let annotations = Regex::new(r" *//(\[[a-z,]+\])?~.*").unwrap();
    annotations.replace_all(rendered, "")
}

pub(crate) fn process(file: &Path, stderr: &[u8]) -> Diagnostics {
    let mut rendered = Vec::new();
    let mut messages = vec![];
    let mut messages_from_unknown_file_or_line = vec![];
    for line in stderr.lines_with_terminator() {
        if line.starts_with_str(b"{") {
            match serde_json::from_slice::<RustcMessage>(line) {
                Ok(msg) => {
                    rendered.extend(
                        filter_annotations_from_rendered(msg.rendered.as_ref().unwrap()).as_bytes(),
                    );
                    msg.insert_recursive(
                        file,
                        &mut messages,
                        &mut messages_from_unknown_file_or_line,
                        None,
                    );
                }
                Err(_) => {
                    // FIXME: add a way to swap out the `process` function, so that cargo can use a different one from rustc
                    // The RustcMessage json just happens to match between the two
                }
            }
        } else {
            // FIXME: do we want to throw interpreter stderr into a separate file?
            rendered.extend(line);
        }
    }
    Diagnostics {
        rendered,
        messages,
        messages_from_unknown_file_or_line,
    }
}
