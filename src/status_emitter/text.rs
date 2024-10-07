use super::RevisionStyle;
use super::StatusEmitter;
use super::Summary;
use super::TestStatus;
use crate::diagnostics::Level;
use crate::diagnostics::Message;
use crate::display;
use crate::parser::Pattern;
use crate::test_result::Errored;
use crate::test_result::TestOk;
use crate::test_result::TestResult;
use crate::Error;
use crate::Errors;
use crate::Format;
use annotate_snippets::Renderer;
use annotate_snippets::Snippet;
use colored::Colorize;
use crossbeam_channel::Sender;
use crossbeam_channel::TryRecvError;
use indicatif::MultiProgress;
use indicatif::ProgressBar;
use indicatif::ProgressDrawTarget;
use indicatif::ProgressStyle;
use spanned::Span;
use std::fmt::{Debug, Display};
use std::io::Write as _;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread::JoinHandle;
use std::time::Duration;

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
    handle: Arc<JoinOnDrop>,
}

struct JoinOnDrop(Mutex<Option<JoinHandle<()>>>);
impl From<JoinHandle<()>> for JoinOnDrop {
    fn from(handle: JoinHandle<()>) -> Self {
        Self(Mutex::new(Some(handle)))
    }
}
impl Drop for JoinOnDrop {
    fn drop(&mut self) {
        self.join();
    }
}

impl JoinOnDrop {
    fn join(&self) {
        let Ok(Some(handle)) = self.0.try_lock().map(|mut g| g.take()) else {
            return;
        };
        let _ = handle.join();
    }
}

#[derive(Debug)]
enum Msg {
    Pop {
        new_leftover_msg: String,
        id: usize,
    },
    Push {
        id: usize,
        parent: usize,
        msg: String,
    },
    Finish,
    Abort,
}

impl Text {
    fn start_thread(progress: OutputVerbosity) -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let handle = std::thread::spawn(move || {
            let bars = MultiProgress::new();
            let progress = match progress {
                OutputVerbosity::Progress => bars.add(ProgressBar::new(0)),
                OutputVerbosity::DiffOnly | OutputVerbosity::Full => {
                    ProgressBar::with_draw_target(Some(0), ProgressDrawTarget::hidden())
                }
            };

            struct Thread {
                parent: usize,
                spinner: ProgressBar,
                /// Used for sanity assertions only
                done: bool,
            }

            impl Debug for Thread {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.debug_struct("Thread")
                        .field("parent", &self.parent)
                        .field(
                            "spinner",
                            &format_args!("{}: {}", self.spinner.prefix(), self.spinner.message()),
                        )
                        .field("done", &self.done)
                        .finish()
                }
            }

            struct ProgressHandler {
                threads: Vec<Option<Thread>>,
                aborted: bool,
                bars: MultiProgress,
            }

            impl ProgressHandler {
                fn parents(&self, mut id: usize) -> impl Iterator<Item = usize> + '_ {
                    std::iter::from_fn(move || {
                        let parent = self.threads[id].as_ref().unwrap().parent;
                        if parent == 0 {
                            None
                        } else {
                            id = parent;
                            Some(parent)
                        }
                    })
                }

                fn root(&self, id: usize) -> usize {
                    self.parents(id).last().unwrap_or(id)
                }

                fn tree(&self, id: usize) -> impl Iterator<Item = (usize, &Thread)> {
                    let root = self.root(id);
                    // No need to look at the entries before `root`, as child nodes
                    // are always after parent nodes.
                    self.threads
                        .iter()
                        .filter_map(|t| t.as_ref())
                        .enumerate()
                        .skip(root - 1)
                        .filter(move |&(i, t)| {
                            root == if t.parent == 0 {
                                i
                            } else {
                                self.root(t.parent)
                            }
                        })
                }

                fn tree_done(&self, id: usize) -> bool {
                    self.tree(id).all(|(_, t)| t.done)
                }

                fn pop(&mut self, new_leftover_msg: String, id: usize) {
                    assert_ne!(id, 0);
                    let Some(Some(thread)) = self.threads.get_mut(id) else {
                        // This can happen when a test was not run at all, because it failed directly during
                        // comment parsing.
                        return;
                    };
                    thread.done = true;
                    let spinner = thread.spinner.clone();
                    spinner.finish_with_message(new_leftover_msg);
                    let progress = &self.threads[0].as_ref().unwrap().spinner;
                    progress.inc(1);
                    if self.tree_done(id) {
                        for (_, thread) in self.tree(id) {
                            self.bars.remove(&thread.spinner);
                            if progress.is_hidden() {
                                self.bars
                                    .println(format!(
                                        "{} {}",
                                        thread.spinner.prefix(),
                                        thread.spinner.message()
                                    ))
                                    .unwrap();
                            }
                        }
                    }
                }

                fn push(&mut self, parent: usize, id: usize, mut msg: String) {
                    assert!(parent < id);
                    self.threads[0].as_mut().unwrap().spinner.inc_length(1);
                    if self.threads.len() <= id {
                        self.threads.resize_with(id + 1, || None);
                    }
                    let parents = if parent == 0 {
                        0
                    } else {
                        self.parents(parent).count() + 1
                    };
                    for _ in 0..parents {
                        msg.insert_str(0, "  ");
                    }
                    let spinner = ProgressBar::new_spinner().with_prefix(msg);
                    let spinner = if parent == 0 {
                        self.bars.add(spinner)
                    } else {
                        let last = self
                            .threads
                            .iter()
                            .enumerate()
                            .rev()
                            .filter_map(|(i, t)| Some((i, t.as_ref()?)))
                            .find(|&(i, _)| self.parents(i).any(|p| p == parent))
                            .map(|(_, thread)| thread)
                            .unwrap_or_else(|| self.threads[parent].as_ref().unwrap());
                        self.bars.insert_after(&last.spinner, spinner)
                    };
                    spinner.set_style(
                        ProgressStyle::with_template("{prefix} {spinner}{msg}").unwrap(),
                    );
                    let thread = &mut self.threads[id];
                    assert!(thread.is_none());
                    let _ = thread.insert(Thread {
                        parent,
                        spinner,
                        done: false,
                    });
                }

                fn tick(&self) {
                    for thread in self.threads.iter().flatten() {
                        if !thread.done {
                            thread.spinner.tick();
                        }
                    }
                }
            }

            impl Drop for ProgressHandler {
                fn drop(&mut self) {
                    let progress = self.threads[0].as_ref().unwrap();
                    for (key, thread) in self.threads.iter().skip(1).enumerate() {
                        if let Some(thread) = thread {
                            assert!(
                                thread.done,
                                "{key} ({}: {}) not finished",
                                thread.spinner.prefix(),
                                thread.spinner.message()
                            );
                        }
                    }
                    if self.aborted {
                        progress.spinner.abandon();
                    } else {
                        assert_eq!(
                            Some(progress.spinner.position()),
                            progress.spinner.length(),
                            "{:?}",
                            self.threads
                        );
                        progress.spinner.finish();
                    }
                }
            }

            let mut handler = ProgressHandler {
                threads: vec![Some(Thread {
                    parent: 0,
                    spinner: progress,
                    done: false,
                })],
                aborted: false,
                bars,
            };

            'outer: loop {
                std::thread::sleep(Duration::from_millis(100));
                loop {
                    match receiver.try_recv() {
                        Ok(val) => match val {
                            Msg::Pop {
                                id,
                                new_leftover_msg,
                            } => {
                                handler.pop(new_leftover_msg, id);
                            }

                            Msg::Push { parent, msg, id } => {
                                handler.push(parent, id, msg);
                            }
                            Msg::Finish => break 'outer,
                            Msg::Abort => handler.aborted = true,
                        },
                        // Sender panicked, skip asserts
                        Err(TryRecvError::Disconnected) => return,
                        Err(TryRecvError::Empty) => break,
                    }
                }
                handler.tick()
            }
        });
        Self {
            sender,
            progress,
            handle: Arc::new(handle.into()),
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
    parent: usize,
    id: usize,
    path: PathBuf,
    revision: String,
    style: RevisionStyle,
}

static ID_GENERATOR: AtomicUsize = AtomicUsize::new(1);

impl TestStatus for TextTest {
    fn done(&self, result: &TestResult, aborted: bool) {
        if aborted {
            self.text.sender.send(Msg::Abort).unwrap();
        }
        let result = match result {
            _ if aborted => "aborted".white(),
            Ok(TestOk::Ok) => "ok".green(),
            Err(Errored { .. }) => "FAILED".bright_red().bold(),
            Ok(TestOk::Ignored) => "ignored (in-test comment)".yellow(),
        };
        let new_leftover_msg = format!("... {result}");
        if ProgressDrawTarget::stdout().is_hidden() {
            match self.style {
                RevisionStyle::Separate => println!("{} {new_leftover_msg}", self.revision),
                RevisionStyle::Show => {
                    let revision = if self.revision.is_empty() {
                        String::new()
                    } else {
                        format!(" (revision `{}`)", self.revision)
                    };
                    println!("{}{revision} {new_leftover_msg}", display(&self.path));
                }
            }
            std::io::stdout().flush().unwrap();
        }
        self.text
            .sender
            .send(Msg::Pop {
                id: self.id,
                new_leftover_msg,
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
        let text = Self {
            text: self.text.clone(),
            path: self.path.clone(),
            parent: self.id,
            id: ID_GENERATOR.fetch_add(1, Ordering::Relaxed),
            revision: revision.to_owned(),
            style,
        };
        // We already created the base entry
        if !revision.is_empty() {
            self.text
                .sender
                .send(Msg::Push {
                    parent: text.parent,
                    id: text.id,
                    msg: text.revision.clone(),
                })
                .unwrap();
        }

        Box::new(text)
    }

    fn for_path(&self, path: &Path) -> Box<dyn TestStatus> {
        let text = Self {
            text: self.text.clone(),
            path: path.to_path_buf(),
            parent: self.id,
            id: ID_GENERATOR.fetch_add(1, Ordering::Relaxed),
            revision: String::new(),
            style: RevisionStyle::Show,
        };

        self.text
            .sender
            .send(Msg::Push {
                id: text.id,
                parent: text.parent,
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
        let id = ID_GENERATOR.fetch_add(1, Ordering::Relaxed);
        self.sender
            .send(Msg::Push {
                id,
                parent: 0,
                msg: display(&path),
            })
            .unwrap();
        Box::new(TextTest {
            text: self.clone(),
            parent: 0,
            id,
            path,
            revision: String::new(),
            style: RevisionStyle::Show,
        })
    }

    fn finalize(
        &self,
        _failures: usize,
        succeeded: usize,
        ignored: usize,
        filtered: usize,
        aborted: bool,
    ) -> Box<dyn Summary> {
        self.sender.send(Msg::Finish).unwrap();

        self.handle.join();
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
            aborted: bool,
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
                    if self.aborted {
                        print!("test result: cancelled.");
                    } else {
                        print!("test result: {}.", "ok".green());
                    }
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
            aborted,
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
