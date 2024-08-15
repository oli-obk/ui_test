//! Variaous schemes for reporting messages during testing or after testing is done.

use annotate_snippets::{Renderer, Snippet};
use bstr::ByteSlice;
use colored::Colorize;
use crossbeam_channel::{Sender, TryRecvError};
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use spanned::{Span, Spanned};

use crate::{
    diagnostics::{Level, Message},
    display, github_actions,
    parser::Pattern,
    test_result::{Errored, TestOk, TestResult},
    Error, Errors, Format,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write as _},
    io::Write as _,
    num::NonZeroUsize,
    panic::{AssertUnwindSafe, RefUnwindSafe},
    path::{Path, PathBuf},
    sync::{atomic::AtomicBool, Arc},
    thread::JoinHandle,
    time::Duration,
};

/// A generic way to handle the output of this crate.
pub trait StatusEmitter: Sync + RefUnwindSafe {
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

/// Some configuration options for revisions
#[derive(Debug, Clone, Copy)]
pub enum RevisionStyle {
    /// Things like dependencies or aux files building are noise in non-interactif mode
    /// and thus silenced.
    Quiet,
    /// Always show them, even if rendering to a file
    Show,
    /// The parent, only show in indicatif mode and on failure
    Parent,
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

#[derive(Clone, Copy)]
enum OutputVerbosity {
    Progress,
    DiffOnly,
    Full,
}

/// A human readable output emitter.
#[derive(Clone)]
pub struct Text {
    sender: Sender<Msg>,
    progress: OutputVerbosity,
    _handle: Arc<JoinOnDrop>,
}

struct JoinOnDrop(AssertUnwindSafe<Option<JoinHandle<()>>>);
impl From<JoinHandle<()>> for JoinOnDrop {
    fn from(handle: JoinHandle<()>) -> Self {
        Self(AssertUnwindSafe(Some(handle)))
    }
}
impl Drop for JoinOnDrop {
    fn drop(&mut self) {
        let _ = self.0 .0.take().unwrap().join();
    }
}

#[derive(Debug)]
enum Msg {
    Pop {
        msg: String,
        new_leftover_msg: Option<String>,
        parent: String,
    },
    Push {
        parent: String,
        msg: String,
    },
    Inc,
    IncLength,
    Finish,
}

impl Text {
    fn insert_parent_and_reposition_children(
        parent: ProgressBar,
        bars: &MultiProgress,
        children: &HashMap<String, (ProgressBar, bool)>,
    ) {
        let parent = bars.insert(0, parent);
        for (msg, (child, _)) in children.iter() {
            if !msg.is_empty() {
                bars.remove(child);
                bars.insert_after(&parent, child.clone());
            }
        }
    }
    fn start_thread(progress: OutputVerbosity) -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let handle = std::thread::spawn(move || {
            let bars = MultiProgress::new();
            let mut progress = None;
            // The bools signal whether the progress bar is done (used for sanity assertions only)
            let mut threads: HashMap<String, HashMap<String, (ProgressBar, bool)>> = HashMap::new();
            'outer: loop {
                std::thread::sleep(Duration::from_millis(100));
                loop {
                    match receiver.try_recv() {
                        Ok(val) => match val {
                            Msg::Pop {
                                msg,
                                new_leftover_msg: new_msg,
                                parent,
                            } => {
                                let Some(children) = threads.get_mut(&parent) else {
                                    // This can happen when a test was not run at all, because it failed directly during
                                    // comment parsing.
                                    continue;
                                };
                                let Some((spinner, done)) = children.get_mut(&msg) else {
                                    panic!("pop: {parent}({msg}): {children:#?}")
                                };
                                *done = true;
                                let template = if msg.is_empty() {
                                    "{prefix} {msg}"
                                } else {
                                    "  {prefix} {msg}"
                                };

                                spinner.set_style(ProgressStyle::with_template(template).unwrap());
                                if let Some(new_msg) = new_msg {
                                    bars.remove(spinner);
                                    let spinner = spinner.clone();
                                    let spinner = if msg.is_empty() {
                                        // insert at the top
                                        Self::insert_parent_and_reposition_children(
                                            spinner.clone(),
                                            &bars,
                                            children,
                                        );

                                        spinner
                                    } else {
                                        let parent = children[""].0.clone();
                                        parent.inc(1);
                                        if children.values().all(|&(_, done)| done) {
                                            bars.remove(&parent);
                                            Self::insert_parent_and_reposition_children(
                                                parent, &bars, children,
                                            );
                                            spinner
                                        } else {
                                            bars.insert_after(&parent, spinner)
                                        }
                                    };
                                    spinner.tick();
                                    spinner.finish_with_message(new_msg);
                                } else {
                                    spinner.finish_and_clear();
                                }
                            }

                            Msg::Push { parent, msg } => {
                                let children = threads.entry(parent.clone()).or_default();
                                if !msg.is_empty() {
                                    let parent = &children
                                        .entry(String::new())
                                        .or_insert_with(|| {
                                            let spinner = bars.add(
                                                ProgressBar::new_spinner().with_prefix(parent),
                                            );
                                            spinner.set_style(
                                                ProgressStyle::with_template(
                                                    "{prefix} {pos}/{len} {msg}",
                                                )
                                                .unwrap(),
                                            );
                                            (spinner, true)
                                        })
                                        .0;
                                    parent.inc_length(1);
                                    let spinner = bars.insert_after(
                                        parent,
                                        ProgressBar::new_spinner().with_prefix(msg.clone()),
                                    );
                                    spinner.set_style(
                                        ProgressStyle::with_template("  {prefix} {spinner} {msg}")
                                            .unwrap(),
                                    );
                                    children.insert(msg, (spinner, false));
                                } else {
                                    let spinner =
                                        bars.add(ProgressBar::new_spinner().with_prefix(parent));
                                    spinner.set_style(
                                        ProgressStyle::with_template("{prefix} {spinner} {msg}")
                                            .unwrap(),
                                    );
                                    children.insert(msg, (spinner, false));
                                };
                            }
                            Msg::IncLength => {
                                progress
                                    .get_or_insert_with(|| bars.add(ProgressBar::new(0)))
                                    .inc_length(1);
                            }
                            Msg::Inc => {
                                progress.as_ref().unwrap().inc(1);
                            }
                            Msg::Finish => break 'outer,
                        },
                        // Sender panicked, skip asserts
                        Err(TryRecvError::Disconnected) => return,
                        Err(TryRecvError::Empty) => break,
                    }
                }
                for children in threads.values() {
                    for (spinner, _done) in children.values() {
                        spinner.tick();
                    }
                }
                if let Some(progress) = &progress {
                    progress.tick()
                }
            }
            for (key, children) in threads.iter() {
                for (sub_key, (_child, done)) in children {
                    assert!(done, "{key} ({sub_key}) not finished");
                }
            }
            if let Some(progress) = progress {
                progress.tick();
                assert!(progress.is_finished());
            }
        });
        Self {
            sender,
            progress,
            _handle: Arc::new(handle.into()),
        }
    }

    /// Print one line per test that gets run.
    pub fn verbose() -> Self {
        Self::start_thread(OutputVerbosity::Full)
    }
    /// Print one line per test that gets run.
    pub fn diff() -> Self {
        Self::start_thread(OutputVerbosity::DiffOnly)
    }
    /// Print a progress bar.
    pub fn quiet() -> Self {
        Self::start_thread(OutputVerbosity::Progress)
    }

    fn is_quiet_output(&self) -> bool {
        matches!(self.progress, OutputVerbosity::Progress)
    }

    fn is_full_output(&self) -> bool {
        matches!(self.progress, OutputVerbosity::Full)
    }
}

impl From<Format> for Text {
    fn from(format: Format) -> Self {
        match format {
            Format::Terse => Text::quiet(),
            Format::Pretty => Text::verbose(),
        }
    }
}

struct TextTest {
    text: Text,
    parent: String,
    path: PathBuf,
    revision: String,
    first: AtomicBool,
    style: RevisionStyle,
}

impl TestStatus for TextTest {
    fn done(&self, result: &TestResult) {
        let new_leftover_msg = if self.text.is_quiet_output() {
            self.text.sender.send(Msg::Inc).unwrap();
            None
        } else {
            let result = match result {
                Ok(TestOk::Ok) => "ok".green(),
                Err(Errored { .. }) => "FAILED".bright_red().bold(),
                Ok(TestOk::Ignored) => "ignored (in-test comment)".yellow(),
            };
            let msg = format!("... {result}");
            if ProgressDrawTarget::stdout().is_hidden() {
                match self.style {
                    RevisionStyle::Quiet => {}
                    RevisionStyle::Show | RevisionStyle::Parent => {
                        let revision = if self.revision.is_empty() {
                            String::new()
                        } else {
                            format!(" (revision `{}`)", self.revision)
                        };
                        println!("{}{revision} {msg}", display(&self.path));
                        std::io::stdout().flush().unwrap();
                    }
                }
            }
            Some(msg)
        };
        self.text
            .sender
            .send(Msg::Pop {
                msg: if self.revision.is_empty() && display(&self.path) != self.parent {
                    display(&self.path)
                } else {
                    self.revision.clone()
                },
                new_leftover_msg,
                parent: self.parent.clone(),
            })
            .unwrap();
    }

    fn failed_test<'a>(
        &self,
        cmd: &str,
        stderr: &'a [u8],
        stdout: &'a [u8],
    ) -> Box<dyn Debug + 'a> {
        let maybe_revision = if self.revision.is_empty() {
            String::new()
        } else {
            format!(" (revision `{}`)", self.revision)
        };
        let text = format!(
            "{} {}{}",
            "FAILED TEST:".bright_red(),
            display(&self.path),
            maybe_revision
        );

        println!();
        println!("{}", text.bold().underline());
        println!("command: {cmd}");
        println!();

        if self.text.is_full_output() {
            #[derive(Debug)]
            struct Guard<'a> {
                stderr: &'a [u8],
                stdout: &'a [u8],
            }
            impl<'a> Drop for Guard<'a> {
                fn drop(&mut self) {
                    println!("{}", "full stderr:".bold());
                    std::io::stdout().write_all(self.stderr).unwrap();
                    println!();
                    println!("{}", "full stdout:".bold());
                    std::io::stdout().write_all(self.stdout).unwrap();
                    println!();
                    println!();
                }
            }
            Box::new(Guard { stderr, stdout })
        } else {
            Box::new(())
        }
    }

    fn path(&self) -> &Path {
        &self.path
    }

    fn for_revision(&self, revision: &str, style: RevisionStyle) -> Box<dyn TestStatus> {
        if !self.first.swap(false, std::sync::atomic::Ordering::Relaxed)
            && self.text.is_quiet_output()
        {
            self.text.sender.send(Msg::IncLength).unwrap();
        }

        let text = Self {
            text: self.text.clone(),
            path: self.path.clone(),
            parent: self.parent.clone(),
            revision: revision.to_owned(),
            first: AtomicBool::new(false),
            style,
        };
        self.text
            .sender
            .send(Msg::Push {
                parent: self.parent.clone(),
                msg: if revision.is_empty() && display(&self.path) != self.parent {
                    display(&self.path)
                } else {
                    text.revision.clone()
                },
            })
            .unwrap();

        Box::new(text)
    }

    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        let text = Self {
            text: self.text.clone(),
            path: path.to_path_buf(),
            parent: self.parent.clone(),
            revision: String::new(),
            first: AtomicBool::new(false),
            style: RevisionStyle::Show,
        };

        self.text
            .sender
            .send(Msg::Push {
                parent: self.parent.clone(),
                msg: display(path),
            })
            .unwrap();
        Box::new(text)
    }

    fn revision(&self) -> &str {
        &self.revision
    }
}

impl StatusEmitter for Text {
    fn register_test(&self, path: PathBuf) -> Box<dyn TestStatus> {
        if self.is_quiet_output() {
            self.sender.send(Msg::IncLength).unwrap();
        }
        Box::new(TextTest {
            text: self.clone(),
            parent: display(&path),
            path,
            revision: String::new(),
            first: AtomicBool::new(true),
            style: RevisionStyle::Parent,
        })
    }

    fn finalize(
        &self,
        _failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
    ) -> Box<dyn Summary> {
        self.sender.send(Msg::Finish).unwrap();
        while !self.sender.is_empty() {
            std::thread::sleep(Duration::from_millis(10));
        }
        if !ProgressDrawTarget::stdout().is_hidden() {
            // The progress bars do not have a trailing newline, so let's
            // add it here.
            println!();
        }
        // Print all errors in a single thread to show reliable output
        struct Summarizer {
            failures: Vec<String>,
            succeeded: usize,
            ignored: usize,
            filtered: usize,
        }

        impl Summary for Summarizer {
            fn test_failure(&mut self, status: &dyn TestStatus, errors: &Errors) {
                for error in errors {
                    print_error(error, status.path());
                }

                self.failures.push(if status.revision().is_empty() {
                    format!("    {}", display(status.path()))
                } else {
                    format!(
                        "    {} (revision {})",
                        display(status.path()),
                        status.revision()
                    )
                });
            }
        }

        impl Drop for Summarizer {
            fn drop(&mut self) {
                if self.failures.is_empty() {
                    println!();
                    print!("test result: {}.", "ok".green());
                } else {
                    println!("{}", "FAILURES:".bright_red().underline().bold());
                    for line in &self.failures {
                        println!("{line}");
                    }
                    println!();
                    print!("test result: {}.", "FAIL".bright_red());
                    print!(" {} failed", self.failures.len().to_string().green());
                    if self.succeeded > 0 || self.ignored > 0 || self.filtered > 0 {
                        print!(";");
                    }
                }
                if self.succeeded > 0 {
                    print!(" {} passed", self.succeeded.to_string().green());
                    if self.ignored > 0 || self.filtered > 0 {
                        print!(";");
                    }
                }
                if self.ignored > 0 {
                    print!(" {} ignored", self.ignored.to_string().yellow());
                    if self.filtered > 0 {
                        print!(";");
                    }
                }
                if self.filtered > 0 {
                    print!(" {} filtered out", self.filtered.to_string().yellow());
                }
                println!();
                println!();
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

fn print_error(error: &Error, path: &Path) {
    /// Every error starts with a header like that, to make them all easy to find.
    /// It is made to look like the headers printed for spanned errors.
    fn print_error_header(msg: impl Display) {
        let text = format!("{} {msg}", "error:".bright_red());
        println!("{}", text.bold());
    }

    match error {
        Error::ExitStatus {
            status,
            expected,
            reason,
        } => {
            // `status` prints as `exit status: N`.
            create_error(
                format!("test got {status}, but expected {expected}"),
                &[&[(reason, reason.span.clone())]],
                path,
            )
        }
        Error::Command { kind, status } => {
            // `status` prints as `exit status: N`.
            print_error_header(format_args!("{kind} failed with {status}"));
        }
        Error::PatternNotFound {
            pattern,
            expected_line,
        } => {
            let line = match expected_line {
                Some(line) => format!("on line {line}"),
                None => format!("outside the testfile"),
            };
            let msg = match &**pattern {
                Pattern::SubString(s) => {
                    format!("`{s}` not found in diagnostics {line}")
                }
                Pattern::Regex(r) => {
                    format!("`/{r}/` does not match diagnostics {line}",)
                }
            };
            // This will print a suitable error header.
            create_error(
                msg,
                &[&[("expected because of this pattern", pattern.span())]],
                path,
            );
        }
        Error::CodeNotFound {
            code,
            expected_line,
        } => {
            let line = match expected_line {
                Some(line) => format!("on line {line}"),
                None => format!("outside the testfile"),
            };
            create_error(
                format!("diagnostic code `{}` not found {line}", &**code),
                &[&[("expected because of this pattern", code.span())]],
                path,
            );
        }
        Error::NoPatternsFound => {
            print_error_header("expected error patterns, but found none");
        }
        Error::PatternFoundInPassTest { mode, span } => {
            let annot = [("expected because of this annotation", span.clone())];
            let mut lines: Vec<&[_]> = vec![&annot];
            let annot = [("expected because of this mode change", mode.clone())];
            if !mode.is_dummy() {
                lines.push(&annot)
            }
            // This will print a suitable error header.
            create_error("error pattern found in pass test", &lines, path);
        }
        Error::OutputDiffers {
            path: output_path,
            actual,
            expected,
            bless_command,
        } => {
            print_error_header("actual output differed from expected");
            if let Some(bless_command) = bless_command {
                println!(
                    "Execute `{}` to update `{}` to the actual output",
                    bless_command,
                    display(output_path)
                );
            }
            println!("{}", format!("--- {}", display(output_path)).red());
            println!(
                "{}",
                format!(
                    "+++ <{} output>",
                    output_path.extension().unwrap().to_str().unwrap()
                )
                .green()
            );
            crate::diff::print_diff(expected, actual);
        }
        Error::ErrorsWithoutPattern { path, msgs } => {
            if let Some((path, _)) = path.as_ref() {
                let msgs = msgs
                    .iter()
                    .map(|msg| {
                        let text = match (&msg.code, msg.level) {
                            (Some(code), Level::Error) => {
                                format!("Error[{code}]: {}", msg.message)
                            }
                            _ => format!("{:?}: {}", msg.level, msg.message),
                        };
                        (text, msg.span.clone().unwrap_or_default())
                    })
                    .collect::<Vec<_>>();
                // This will print a suitable error header.
                create_error(
                    format!("there were {} unmatched diagnostics", msgs.len()),
                    &[&msgs
                        .iter()
                        .map(|(msg, lc)| (msg.as_ref(), lc.clone()))
                        .collect::<Vec<_>>()],
                    path,
                );
            } else {
                print_error_header(format_args!(
                    "there were {} unmatched diagnostics that occurred outside the testfile and had no pattern",
                    msgs.len(),
                ));
                for Message {
                    level,
                    message,
                    line: _,
                    code: _,
                    span: _,
                } in msgs
                {
                    println!("    {level:?}: {message}")
                }
            }
        }
        Error::InvalidComment { msg, span } => {
            // This will print a suitable error header.
            create_error(msg, &[&[("", span.clone())]], path)
        }
        Error::MultipleRevisionsWithResults { kind, lines } => {
            let title = format!("multiple {kind} found");
            // This will print a suitable error header.
            create_error(
                title,
                &lines.iter().map(|_line| &[] as &[_]).collect::<Vec<_>>(),
                path,
            )
        }
        Error::Bug(msg) => {
            print_error_header("a bug in `ui_test` occurred");
            println!("{msg}");
        }
        Error::Aux {
            path: aux_path,
            errors,
        } => {
            create_error(
                "aux build failed",
                &[&[(&path.display().to_string(), aux_path.span.clone())]],
                &aux_path.span.file,
            );
            for error in errors {
                print_error(error, aux_path);
            }
        }
        Error::Rustfix(error) => {
            print_error_header(format_args!(
                "failed to apply suggestions for {} with rustfix",
                display(path)
            ));
            println!("{error}");
            println!("Add //@no-rustfix to the test file to ignore rustfix suggestions");
        }
        Error::ConfigError(msg) => println!("{msg}"),
    }
    println!();
}

#[allow(clippy::type_complexity)]
fn create_error(s: impl AsRef<str>, lines: &[&[(&str, Span)]], file: &Path) {
    let source = std::fs::read_to_string(file).unwrap();
    let file = display(file);
    let mut msg = annotate_snippets::Level::Error.title(s.as_ref());
    for &label in lines {
        let annotations = label
            .iter()
            .filter(|(_, span)| !span.is_dummy())
            .map(|(label, span)| {
                annotate_snippets::Level::Error
                    .span(span.bytes.clone())
                    .label(label)
            })
            .collect::<Vec<_>>();
        if !annotations.is_empty() {
            let snippet = Snippet::source(&source)
                .fold(true)
                .origin(&file)
                .annotations(annotations);
            msg = msg.snippet(snippet);
        }
        let footer = label
            .iter()
            .filter(|(_, span)| span.is_dummy())
            .map(|(label, _)| annotate_snippets::Level::Note.title(label));
        msg = msg.footers(footer);
    }
    let renderer = if colored::control::SHOULD_COLORIZE.should_colorize() {
        Renderer::styled()
    } else {
        Renderer::plain()
    };
    println!("{}", renderer.render(msg));
}

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
                prettydiff::diff_lines(expected.to_str().unwrap(), actual.to_str().unwrap()).diff()
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
                        let mut err =
                            github_actions::error(display(output_path), "missing line in output")
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

impl<T: TestStatus, U: TestStatus> TestStatus for (T, U) {
    fn done(&self, result: &TestResult) {
        self.0.done(result);
        self.1.done(result);
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
