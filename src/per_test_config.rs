//! This module allows you to configure the default settings for all
//! tests. All data structures here are normally parsed from `@` comments
//! in the files. These comments still overwrite the defaults, although
//! some boolean settings have no way to disable them.

use bstr::ByteSlice;
use std::collections::HashSet;
use std::ffi::OsString;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use spanned::{Span, Spanned};

use crate::dependencies::{Build, BuildManager};
pub use crate::parser::{Comments, Condition, Revisioned};
use crate::parser::{ErrorMatch, ErrorMatchKind, OptWithLine};
pub use crate::rustc_stderr::Level;
use crate::rustc_stderr::Message;
use crate::test_result::{Errored, TestOk, TestResult};
use crate::{
    core::strip_path_prefix, rustc_stderr, Config, Error, Errors, Mode, OutputConflictHandling,
    RustfixMode,
};

pub(crate) struct TestConfig<'a> {
    pub config: Config,
    pub revision: &'a str,
    pub comments: &'a Comments,
    pub path: &'a Path,
}

impl TestConfig<'_> {
    pub fn patch_out_dir(&mut self) {
        // Put aux builds into a separate directory per path so that multiple aux files
        // from different directories (but with the same file name) don't collide.
        let relative = strip_path_prefix(self.path.parent().unwrap(), &self.config.out_dir);

        self.config.out_dir.extend(relative);
    }

    pub fn extension(&self, extension: &str) -> String {
        if self.revision.is_empty() {
            extension.to_string()
        } else {
            format!("{}.{extension}", self.revision)
        }
    }

    pub fn mode(&self) -> Result<Spanned<Mode>, Errored> {
        self.comments.mode(self.revision)
    }

    pub fn edition(&self) -> Result<Option<Spanned<String>>, Errored> {
        self.comments.edition(self.revision)
    }

    pub fn find_one<'a, T: 'a>(
        &'a self,
        kind: &str,
        f: impl Fn(&'a Revisioned) -> OptWithLine<T>,
    ) -> Result<OptWithLine<T>, Errored> {
        self.comments.find_one_for_revision(self.revision, kind, f)
    }

    pub fn comments(&self) -> impl Iterator<Item = &'_ Revisioned> {
        self.comments.for_revision(self.revision)
    }

    pub fn collect<'a, T, I: Iterator<Item = T>, R: FromIterator<T>>(
        &'a self,
        f: impl Fn(&'a Revisioned) -> I,
    ) -> R {
        self.comments().flat_map(f).collect()
    }

    pub fn build_command(&self) -> Result<Command, Errored> {
        let TestConfig {
            config,
            revision,
            comments,
            path,
        } = self;
        let mut cmd = config.program.build(&config.out_dir);
        cmd.arg(path);
        if !revision.is_empty() {
            cmd.arg(format!("--cfg={revision}"));
        }
        for arg in comments
            .for_revision(revision)
            .flat_map(|r| r.compile_flags.iter())
        {
            cmd.arg(arg);
        }
        let edition = comments.edition(revision)?;

        if let Some(edition) = edition {
            cmd.arg("--edition").arg(&*edition);
        }

        if let Some(target) = &config.target {
            // Adding a `--target` arg to calls to Cargo will cause target folders
            // to create a target-specific sub-folder. We can avoid that by just
            // not passing a `--target` arg if its the same as the host.
            if !config.host_matches_target() {
                cmd.arg("--target").arg(target);
            }
        }

        // False positive in miri, our `map` uses a ref pattern to get the references to the tuple fields instead
        // of a reference to a tuple
        #[allow(clippy::map_identity)]
        cmd.envs(
            comments
                .for_revision(revision)
                .flat_map(|r| r.env_vars.iter())
                .map(|(k, v)| (k, v)),
        );

        Ok(cmd)
    }

    pub fn output_path(&self, kind: &str) -> PathBuf {
        let ext = self.extension(kind);
        if self.comments().any(|r| r.stderr_per_bitwidth) {
            return self
                .path
                .with_extension(format!("{}bit.{ext}", self.config.get_pointer_width()));
        }
        self.path.with_extension(ext)
    }

    pub fn normalize(&self, text: &[u8], kind: &'static str) -> Vec<u8> {
        let mut text = text.to_owned();

        for (from, to) in self.comments().flat_map(|r| match kind {
            "fixed" => &[] as &[_],
            "stderr" => &r.normalize_stderr,
            "stdout" => &r.normalize_stdout,
            _ => unreachable!(),
        }) {
            text = from.replace_all(&text, to).into_owned();
        }
        text
    }

    pub fn check_test_output(&self, errors: &mut Errors, stdout: &[u8], stderr: &[u8]) {
        // Check output files (if any)
        // Check output files against actual output
        self.check_output(stderr, errors, "stderr");
        self.check_output(stdout, errors, "stdout");
    }

    pub fn check_output(&self, output: &[u8], errors: &mut Errors, kind: &'static str) -> PathBuf {
        let output = self.normalize(output, kind);
        let path = self.output_path(kind);
        match &self.config.output_conflict_handling {
            OutputConflictHandling::Error => {
                let expected_output = std::fs::read(&path).unwrap_or_default();
                if output != expected_output {
                    errors.push(Error::OutputDiffers {
                        path: path.clone(),
                        actual: output.clone(),
                        expected: expected_output,
                        bless_command: self.config.bless_command.clone(),
                    });
                }
            }
            OutputConflictHandling::Bless => {
                if output.is_empty() {
                    let _ = std::fs::remove_file(&path);
                } else {
                    std::fs::write(&path, &output).unwrap();
                }
            }
            OutputConflictHandling::Ignore => {}
        }
        path
    }

    pub fn check_test_result(
        &self,
        command: Command,
        mode: Mode,
        output: Output,
    ) -> Result<(Command, Output), Errored> {
        let mut errors = vec![];
        errors.extend(mode.ok(output.status).err());
        // Always remove annotation comments from stderr.
        let diagnostics = rustc_stderr::process(self.path, &output.stderr);
        self.check_test_output(&mut errors, &output.stdout, &diagnostics.rendered);
        // Check error annotations in the source against output
        self.check_annotations(
            diagnostics.messages,
            diagnostics.messages_from_unknown_file_or_line,
            &mut errors,
        )?;
        if errors.is_empty() {
            Ok((command, output))
        } else {
            Err(Errored {
                command,
                errors,
                stderr: diagnostics.rendered,
                stdout: output.stdout,
            })
        }
    }

    pub fn check_annotations(
        &self,
        mut messages: Vec<Vec<Message>>,
        mut messages_from_unknown_file_or_line: Vec<Message>,
        errors: &mut Errors,
    ) -> Result<(), Errored> {
        let error_patterns = self.comments().flat_map(|r| r.error_in_other_files.iter());

        let mut seen_error_match = None;
        for error_pattern in error_patterns {
            seen_error_match = Some(error_pattern.span());
            // first check the diagnostics messages outside of our file. We check this first, so that
            // you can mix in-file annotations with //@error-in-other-file annotations, even if there is overlap
            // in the messages.
            if let Some(i) = messages_from_unknown_file_or_line
                .iter()
                .position(|msg| error_pattern.matches(&msg.message))
            {
                messages_from_unknown_file_or_line.remove(i);
            } else {
                errors.push(Error::PatternNotFound {
                    pattern: error_pattern.clone(),
                    expected_line: None,
                });
            }
        }
        let diagnostic_code_prefix = self
            .find_one("diagnostic_code_prefix", |r| {
                r.diagnostic_code_prefix.clone()
            })?
            .into_inner()
            .map(|s| s.content)
            .unwrap_or_default();

        // The order on `Level` is such that `Error` is the highest level.
        // We will ensure that *all* diagnostics of level at least `lowest_annotation_level`
        // are matched.
        let mut lowest_annotation_level = Level::Error;
        'err: for &ErrorMatch { ref kind, line } in
            self.comments().flat_map(|r| r.error_matches.iter())
        {
            match kind {
                ErrorMatchKind::Code(code) => {
                    seen_error_match = Some(code.span());
                }
                &ErrorMatchKind::Pattern { ref pattern, level } => {
                    seen_error_match = Some(pattern.span());
                    // If we found a diagnostic with a level annotation, make sure that all
                    // diagnostics of that level have annotations, even if we don't end up finding a matching diagnostic
                    // for this pattern.
                    if lowest_annotation_level > level {
                        lowest_annotation_level = level;
                    }
                }
            }

            if let Some(msgs) = messages.get_mut(line.get()) {
                match kind {
                    &ErrorMatchKind::Pattern { ref pattern, level } => {
                        let found = msgs
                            .iter()
                            .position(|msg| pattern.matches(&msg.message) && msg.level == level);
                        if let Some(found) = found {
                            msgs.remove(found);
                            continue;
                        }
                    }
                    ErrorMatchKind::Code(code) => {
                        for (i, msg) in msgs.iter().enumerate() {
                            if msg.level != Level::Error {
                                continue;
                            }
                            let Some(msg_code) = &msg.code else { continue };
                            let Some(msg) = msg_code.strip_prefix(&diagnostic_code_prefix) else {
                                continue;
                            };
                            if msg == **code {
                                msgs.remove(i);
                                continue 'err;
                            }
                        }
                    }
                }
            }

            errors.push(match kind {
                ErrorMatchKind::Pattern { pattern, .. } => Error::PatternNotFound {
                    pattern: pattern.clone(),
                    expected_line: Some(line),
                },
                ErrorMatchKind::Code(code) => Error::CodeNotFound {
                    code: Spanned::new(
                        format!("{}{}", diagnostic_code_prefix, **code),
                        code.span(),
                    ),
                    expected_line: Some(line),
                },
            });
        }

        let required_annotation_level = self
            .find_one("`require_annotations_for_level` annotations", |r| {
                r.require_annotations_for_level.clone()
            })?;

        let required_annotation_level = required_annotation_level
            .into_inner()
            .map_or(lowest_annotation_level, |l| *l);
        let filter = |mut msgs: Vec<Message>| -> Vec<_> {
            msgs.retain(|msg| msg.level >= required_annotation_level);
            msgs
        };

        let mode = self.mode()?;

        if !matches!(*mode, Mode::Yolo { .. }) {
            let messages_from_unknown_file_or_line = filter(messages_from_unknown_file_or_line);
            if !messages_from_unknown_file_or_line.is_empty() {
                errors.push(Error::ErrorsWithoutPattern {
                    path: None,
                    msgs: messages_from_unknown_file_or_line,
                });
            }

            for (line, msgs) in messages.into_iter().enumerate() {
                let msgs = filter(msgs);
                if !msgs.is_empty() {
                    let line = NonZeroUsize::new(line).expect("line 0 is always empty");
                    errors.push(Error::ErrorsWithoutPattern {
                        path: Some(Spanned::new(
                            self.path.to_path_buf(),
                            spanned::Span {
                                line_start: line,
                                ..spanned::Span::default()
                            },
                        )),
                        msgs,
                    });
                }
            }
        }

        match (*mode, seen_error_match) {
            (Mode::Pass, Some(span)) | (Mode::Panic, Some(span)) => {
                errors.push(Error::PatternFoundInPassTest {
                    mode: mode.span(),
                    span,
                })
            }
            (
                Mode::Fail {
                    require_patterns: true,
                    ..
                },
                None,
            ) => errors.push(Error::NoPatternsFound),
            _ => {}
        }
        Ok(())
    }

    pub fn build_aux_files(
        &self,
        aux_dir: &Path,
        build_manager: &BuildManager<'_>,
    ) -> Result<Vec<OsString>, Errored> {
        let mut extra_args = vec![];
        for rev in self.comments() {
            for aux in &rev.aux_builds {
                let line = aux.line();
                let aux = &**aux;
                let aux_file = if aux.starts_with("..") {
                    aux_dir.parent().unwrap().join(aux)
                } else {
                    aux_dir.join(aux)
                };
                extra_args.extend(
                    build_manager
                        .build(
                            Build::Aux {
                                aux_file: strip_path_prefix(
                                    &aux_file.canonicalize().map_err(|err| Errored {
                                        command: Command::new(format!(
                                            "canonicalizing path `{}`",
                                            aux_file.display()
                                        )),
                                        errors: vec![],
                                        stderr: err.to_string().into_bytes(),
                                        stdout: vec![],
                                    })?,
                                    &std::env::current_dir().unwrap(),
                                )
                                .collect(),
                            },
                            &self.config,
                        )
                        .map_err(
                            |Errored {
                                 command,
                                 errors,
                                 stderr,
                                 stdout,
                             }| Errored {
                                command,
                                errors: vec![Error::Aux {
                                    path: aux_file,
                                    errors,
                                    line,
                                }],
                                stderr,
                                stdout,
                            },
                        )?,
                );
            }
        }
        Ok(extra_args)
    }

    pub fn run_test(mut self, build_manager: &BuildManager<'_>) -> TestResult {
        let extra_args = self.build_aux_files(
            &self.path.parent().unwrap().join("auxiliary"),
            build_manager,
        )?;

        self.patch_out_dir();

        let mut cmd = self.build_command()?;
        cmd.args(&extra_args);
        let stdin = self.path.with_extension(self.extension("stdin"));
        if stdin.exists() {
            cmd.stdin(std::fs::File::open(stdin).unwrap());
        }

        let (cmd, output) = crate::core::run_command(cmd)?;

        let mode = self.mode()?;
        let (cmd, output) = self.check_test_result(
            cmd,
            match *mode {
                Mode::Run { .. } => Mode::Pass,
                _ => *mode,
            },
            output,
        )?;

        if let Mode::Run { .. } = *mode {
            self.run_test_binary(mode, cmd)
        } else {
            self.run_rustfix(output, *mode, extra_args)?;
            Ok(TestOk::Ok)
        }
    }

    fn run_test_binary(self, mode: Spanned<Mode>, mut cmd: Command) -> TestResult {
        let revision = self.extension("run");
        let config = TestConfig {
            config: self.config,
            revision: &revision,
            comments: self.comments,
            path: self.path,
        };
        cmd.arg("--print").arg("file-names");
        let output = cmd.output().unwrap();
        assert!(output.status.success());

        let mut files = output.stdout.lines();
        let file = files.next().unwrap();
        assert_eq!(files.next(), None);
        let file = std::str::from_utf8(file).unwrap();
        let exe_file = config.config.out_dir.join(file);
        let mut exe = Command::new(&exe_file);
        let stdin = config.path.with_extension(format!("{revision}.stdin"));
        if stdin.exists() {
            exe.stdin(std::fs::File::open(stdin).unwrap());
        }
        let output = exe
            .output()
            .unwrap_or_else(|err| panic!("exe file: {}: {err}", exe_file.display()));

        let mut errors = vec![];

        config.check_test_output(&mut errors, &output.stdout, &output.stderr);

        errors.extend(mode.ok(output.status).err());
        if errors.is_empty() {
            Ok(TestOk::Ok)
        } else {
            Err(Errored {
                command: exe,
                errors,
                stderr: vec![],
                stdout: vec![],
            })
        }
    }

    fn run_rustfix(
        self,
        output: Output,
        mode: Mode,
        extra_args: Vec<OsString>,
    ) -> Result<(), Errored> {
        let no_run_rustfix = self.find_one("`no-rustfix` annotations", |r| r.no_rustfix.clone())?;

        let global_rustfix = match mode {
            Mode::Pass | Mode::Run { .. } | Mode::Panic => RustfixMode::Disabled,
            Mode::Fail { rustfix, .. } | Mode::Yolo { rustfix } => rustfix,
        };

        let fixed_code = (no_run_rustfix.is_none() && global_rustfix.enabled())
            .then_some(())
            .and_then(|()| {
                let suggestions = std::str::from_utf8(&output.stderr)
                    .unwrap()
                    .lines()
                    .flat_map(|line| {
                        if !line.starts_with('{') {
                            return vec![];
                        }
                        rustfix::get_suggestions_from_json(
                            line,
                            &HashSet::new(),
                            if global_rustfix == RustfixMode::Everything {
                                rustfix::Filter::Everything
                            } else {
                                rustfix::Filter::MachineApplicableOnly
                            },
                        )
                        .unwrap_or_else(|err| {
                            panic!("could not deserialize diagnostics json for rustfix {err}:{line}")
                        })
                    })
                    .collect::<Vec<_>>();
                if suggestions.is_empty() {
                    None
                } else {
                    let path_str = self.path.display().to_string();
                    for sugg in &suggestions {
                        for snip in &sugg.snippets {
                            if snip.file_name != path_str {
                                return Some(Err(anyhow::anyhow!("cannot apply suggestions for `{}` since main file is `{path_str}`. Please use `//@no-rustfix` to disable rustfix", snip.file_name)));
                            }
                        }
                    }
                    Some(rustfix::apply_suggestions(
                        &std::fs::read_to_string(self.path).unwrap(),
                        &suggestions,
                    ))
                }
            })
            .transpose()
            .map_err(|err| Errored {
                command: Command::new(format!("rustfix {}", self.path.display())),
                errors: vec![Error::Rustfix(err)],
                stderr: output.stderr,
                stdout: output.stdout,
            })?;

        let edition = self.edition()?.into();
        let rustfix_comments = Comments {
            revisions: None,
            revisioned: std::iter::once((
                vec![],
                Revisioned {
                    span: Span::default(),
                    ignore: vec![],
                    only: vec![],
                    stderr_per_bitwidth: false,
                    compile_flags: self.collect(|r| r.compile_flags.iter().cloned()),
                    env_vars: self.collect(|r| r.env_vars.iter().cloned()),
                    normalize_stderr: vec![],
                    normalize_stdout: vec![],
                    error_in_other_files: vec![],
                    error_matches: vec![],
                    require_annotations_for_level: Default::default(),
                    aux_builds: self.collect(|r| r.aux_builds.iter().cloned()),
                    edition,
                    mode: OptWithLine::new(Mode::Pass, Span::default()),
                    no_rustfix: OptWithLine::new((), Span::default()),
                    diagnostic_code_prefix: OptWithLine::new(String::new(), Span::default()),
                    needs_asm_support: false,
                },
            ))
            .collect(),
        };
        let config = TestConfig {
            config: self.config,
            revision: self.revision,
            comments: &rustfix_comments,
            path: self.path,
        };

        let run = fixed_code.is_some();
        let mut errors = vec![];
        let rustfix_path = config.check_output(
            // Always check for `.fixed` files, even if there were reasons not to run rustfix.
            // We don't want to leave around stray `.fixed` files
            fixed_code.unwrap_or_default().as_bytes(),
            &mut errors,
            "fixed",
        );
        // picking the crate name from the file name is problematic when `.revision_name` is inserted,
        // so we compute it here before replacing the path.
        let crate_name = config
            .path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .replace('-', "_");
        let config = TestConfig {
            config: config.config,
            revision: config.revision,
            comments: &rustfix_comments,
            path: &rustfix_path,
        };
        if !errors.is_empty() {
            return Err(Errored {
                command: Command::new(format!("checking {}", config.path.display())),
                errors,
                stderr: vec![],
                stdout: vec![],
            });
        }

        if !run {
            return Ok(());
        }

        let mut cmd = config.build_command()?;
        cmd.args(extra_args);
        cmd.arg("--crate-name").arg(crate_name);
        let output = cmd.output().unwrap();
        if output.status.success() {
            Ok(())
        } else {
            Err(Errored {
                command: cmd,
                errors: vec![Error::Command {
                    kind: "rustfix".into(),
                    status: output.status,
                }],
                stderr: rustc_stderr::process(&rustfix_path, &output.stderr).rendered,
                stdout: output.stdout,
            })
        }
    }
}
