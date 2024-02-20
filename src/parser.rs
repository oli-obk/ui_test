use std::{
    collections::HashMap,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    process::Command,
};

use bstr::{ByteSlice, Utf8Error};
use regex::bytes::Regex;

use crate::{rustc_stderr::Level, Error, Errored, Match, Mode};

use color_eyre::eyre::{Context, Result};

pub(crate) use spanned::*;

mod spanned;
#[cfg(test)]
mod tests;

/// This crate supports various magic comments that get parsed as file-specific
/// configuration values. This struct parses them all in one go and then they
/// get processed by their respective use sites.
#[derive(Debug, Clone)]
pub struct Comments {
    /// List of revision names to execute. Can only be specified once
    pub revisions: Option<Vec<String>>,
    /// Comments that are only available under specific revisions.
    /// The defaults are in key `vec![]`
    pub revisioned: HashMap<Vec<String>, Revisioned>,
}

impl Default for Comments {
    fn default() -> Self {
        let mut this = Self {
            revisions: Default::default(),
            revisioned: Default::default(),
        };
        this.revisioned.insert(vec![], Revisioned::default());
        this
    }
}

impl Comments {
    /// Check that a comment isn't specified twice across multiple differently revisioned statements.
    /// e.g. `//@[foo, bar] error-in-other-file: bop` and `//@[foo, baz] error-in-other-file boop` would end up
    /// specifying two error patterns that are available in revision `foo`.
    pub fn find_one_for_revision<'a, T: 'a>(
        &'a self,
        revision: &'a str,
        kind: &str,
        f: impl Fn(&'a Revisioned) -> OptWithLine<T>,
    ) -> Result<OptWithLine<T>, Errored> {
        let mut result = None;
        let mut errors = vec![];
        for (k, rev) in &self.revisioned {
            if !k.iter().any(|r| r == revision) {
                continue;
            }
            if let Some(found) = f(rev).into_inner() {
                if result.is_some() {
                    errors.push(found.line());
                } else {
                    result = found.into();
                }
            }
        }
        if result.is_none() {
            result = f(&self.revisioned[&[][..]]).into_inner();
        }
        if errors.is_empty() {
            Ok(result.into())
        } else {
            Err(Errored {
                command: Command::new(format!("<finding flags for revision `{revision}`>")),
                errors: vec![Error::MultipleRevisionsWithResults {
                    kind: kind.to_string(),
                    lines: errors,
                }],
                stderr: vec![],
                stdout: vec![],
            })
        }
    }

    /// Returns an iterator over all revisioned comments that match the revision.
    pub fn for_revision<'a>(&'a self, revision: &'a str) -> impl Iterator<Item = &'a Revisioned> {
        self.revisioned.iter().filter_map(move |(k, v)| {
            if k.is_empty() || k.iter().any(|rev| rev == revision) {
                Some(v)
            } else {
                None
            }
        })
    }

    pub(crate) fn edition(&self, revision: &str) -> Result<Option<Spanned<String>>, Errored> {
        let edition = self
            .find_one_for_revision(revision, "`edition` annotations", |r| r.edition.clone())?
            .into_inner();
        Ok(edition)
    }

    /// The comments set for all revisions
    pub fn base(&mut self) -> &mut Revisioned {
        self.revisioned.get_mut(&[][..]).unwrap()
    }

    /// The comments set for all revisions
    pub fn base_immut(&self) -> &Revisioned {
        self.revisioned.get(&[][..]).unwrap()
    }

    pub(crate) fn mode(&self, revision: &str) -> Result<Spanned<Mode>, Errored> {
        let mode = self
            .find_one_for_revision(revision, "`mode` annotations", |r| r.mode.clone())?
            .into_inner()
            .ok_or_else(|| Errored {
                command: Command::new(format!("<finding mode for revision `{revision}`>")),
                errors: vec![Error::ConfigError(
                    "no mode set up in Config::comment_defaults".into(),
                )],
                stderr: vec![],
                stdout: vec![],
            })?;
        Ok(mode)
    }
}

#[derive(Debug, Clone, Default)]
/// Comments that can be filtered for specific revisions.
pub struct Revisioned {
    /// The character range in which this revisioned item was first added.
    /// Used for reporting errors on unknown revisions.
    pub span: Span,
    /// Don't run this test if any of these filters apply
    pub ignore: Vec<Condition>,
    /// Only run this test if all of these filters apply
    pub only: Vec<Condition>,
    /// Generate one .stderr file per bit width, by prepending with `.64bit` and similar
    pub stderr_per_bitwidth: bool,
    /// Additional flags to pass to the executable
    pub compile_flags: Vec<String>,
    /// Additional env vars to set for the executable
    pub env_vars: Vec<(String, String)>,
    /// Normalizations to apply to the stderr output before emitting it to disk
    pub normalize_stderr: Vec<(Match, Vec<u8>)>,
    /// Normalizations to apply to the stdout output before emitting it to disk
    pub normalize_stdout: Vec<(Match, Vec<u8>)>,
    /// Arbitrary patterns to look for in the stderr.
    /// The error must be from another file, as errors from the current file must be
    /// checked via `error_matches`.
    pub(crate) error_in_other_files: Vec<Spanned<Pattern>>,
    pub(crate) error_matches: Vec<ErrorMatch>,
    /// Ignore diagnostics below this level.
    /// `None` means pick the lowest level from the `error_pattern`s.
    pub require_annotations_for_level: OptWithLine<Level>,
    /// Files that get built and exposed as dependencies to the current test.
    pub aux_builds: Vec<Spanned<PathBuf>>,
    /// Set the `--edition` flag on the test.
    pub edition: OptWithLine<String>,
    /// The mode this test is being run in.
    pub mode: OptWithLine<Mode>,
    pub(crate) needs_asm_support: bool,
    /// Don't run [`rustfix`] for this test
    pub no_rustfix: OptWithLine<()>,
    /// Prefix added to all diagnostic code matchers. Note this will make it impossible
    /// match codes which do not contain this prefix.
    pub diagnostic_code_prefix: OptWithLine<String>,
}

#[derive(Debug)]
struct CommentParser<T> {
    /// The comments being built.
    comments: T,
    /// Any errors that ocurred during comment parsing.
    errors: Vec<Error>,
    /// The available commands and their parsing logic
    commands: HashMap<&'static str, CommandParserFunc>,
}

type CommandParserFunc = fn(&mut CommentParser<&mut Revisioned>, args: Spanned<&str>, span: Span);

impl<T> std::ops::Deref for CommentParser<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.comments
    }
}

impl<T> std::ops::DerefMut for CommentParser<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.comments
    }
}

/// The conditions used for "ignore" and "only" filters.
#[derive(Debug, Clone)]
pub enum Condition {
    /// The given string must appear in the host triple.
    Host(String),
    /// The given string must appear in the target triple.
    Target(String),
    /// Tests that the bitwidth is the given one.
    Bitwidth(u8),
    /// Tests that the target is the host.
    OnHost,
}

#[derive(Debug, Clone)]
/// An error pattern parsed from a `//~` comment.
pub enum Pattern {
    SubString(String),
    Regex(Regex),
}

#[derive(Debug, Clone)]
pub(crate) enum ErrorMatchKind {
    /// A level and pattern pair parsed from a `//~ LEVEL: Message` comment.
    Pattern {
        pattern: Spanned<Pattern>,
        level: Level,
    },
    /// An error code parsed from a `//~ error_code` comment.
    Code(Spanned<String>),
}

impl ErrorMatchKind {
    fn span(&self) -> &Span {
        match self {
            Self::Pattern { pattern, .. } => &pattern.span,
            Self::Code(code) => &code.span,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ErrorMatch {
    pub(crate) kind: ErrorMatchKind,
    /// The line this pattern is expecting to find a message in.
    pub line: NonZeroUsize,
}

impl Condition {
    fn parse(c: &str) -> std::result::Result<Self, String> {
        if c == "on-host" {
            Ok(Condition::OnHost)
        } else if let Some(bits) = c.strip_suffix("bit") {
            let bits: u8 = bits.parse().map_err(|_err| {
                format!("invalid ignore/only filter ending in 'bit': {c:?} is not a valid bitwdith")
            })?;
            Ok(Condition::Bitwidth(bits))
        } else if let Some(triple_substr) = c.strip_prefix("target-") {
            Ok(Condition::Target(triple_substr.to_owned()))
        } else if let Some(triple_substr) = c.strip_prefix("host-") {
            Ok(Condition::Host(triple_substr.to_owned()))
        } else {
            Err(format!(
                "`{c}` is not a valid condition, expected `on-host`, /[0-9]+bit/, /host-.*/, or /target-.*/"
            ))
        }
    }
}

enum ParsePatternResult {
    Other,
    ErrorAbove {
        match_line: NonZeroUsize,
    },
    ErrorBelow {
        span: Span,
        match_line: NonZeroUsize,
    },
    Fallthrough {
        span: Span,
        idx: usize,
    },
}

impl Comments {
    pub(crate) fn parse_file(
        comments: Comments,
        path: &Path,
    ) -> Result<std::result::Result<Self, Vec<Error>>> {
        let content =
            std::fs::read(path).wrap_err_with(|| format!("failed to read {}", path.display()))?;
        Ok(Self::parse(&content, comments, path))
    }

    /// Parse comments in `content`.
    /// `path` is only used to emit diagnostics if parsing fails.
    pub(crate) fn parse(
        content: &(impl AsRef<[u8]> + ?Sized),
        comments: Comments,
        file: &Path,
    ) -> std::result::Result<Self, Vec<Error>> {
        let mut parser = CommentParser {
            comments,
            errors: vec![],
            commands: CommentParser::<_>::commands(),
        };

        let defaults = std::mem::take(parser.comments.revisioned.get_mut(&[][..]).unwrap());

        let mut delayed_fallthrough = Vec::new();
        let mut fallthrough_to = None; // The line that a `|` will refer to.
        let mut last_line = 0;
        for (l, line) in content.as_ref().lines().enumerate() {
            last_line = l + 1;
            let l = NonZeroUsize::new(l + 1).unwrap(); // enumerate starts at 0, but line numbers start at 1
            let span = Span {
                file: file.to_path_buf(),
                line_start: l,
                line_end: l,
                col_start: NonZeroUsize::new(1).unwrap(),
                col_end: NonZeroUsize::new(line.chars().count() + 1).unwrap(),
            };
            match parser.parse_checked_line(fallthrough_to, Spanned::new(line, span)) {
                Ok(ParsePatternResult::Other) => {
                    fallthrough_to = None;
                }
                Ok(ParsePatternResult::ErrorAbove { match_line }) => {
                    fallthrough_to = Some(match_line);
                }
                Ok(ParsePatternResult::Fallthrough { span, idx }) => {
                    delayed_fallthrough.push((span, l, idx));
                }
                Ok(ParsePatternResult::ErrorBelow { span, match_line }) => {
                    if fallthrough_to.is_some() {
                        parser.error(
                            span,
                            "`//~v` comment immediately following a `//~^` comment chain",
                        );
                    }

                    for (span, line, idx) in delayed_fallthrough.drain(..) {
                        if let Some(rev) = parser
                            .comments
                            .revisioned
                            .values_mut()
                            .find(|rev| rev.error_matches[idx].kind.span().line_start == line)
                        {
                            rev.error_matches[idx].line = match_line;
                        } else {
                            parser.error(span, "`//~|` comment not attached to anchoring matcher");
                        }
                    }
                }
                Err(e) => parser.error(e.span, format!("Comment is not utf8: {:?}", e.content)),
            }
        }
        if let Some(revisions) = &parser.comments.revisions {
            for (key, revisioned) in &parser.comments.revisioned {
                for rev in key {
                    if !revisions.contains(rev) {
                        parser.errors.push(Error::InvalidComment {
                            msg: format!("the revision `{rev}` is not known"),
                            span: revisioned.span.clone(),
                        })
                    }
                }
            }
        } else {
            for (key, revisioned) in &parser.comments.revisioned {
                if !key.is_empty() {
                    parser.errors.push(Error::InvalidComment {
                        msg: "there are no revisions in this test".into(),
                        span: revisioned.span.clone(),
                    })
                }
            }
        }

        for revisioned in parser.comments.revisioned.values() {
            for m in &revisioned.error_matches {
                if m.line.get() > last_line {
                    let span = match &m.kind {
                        ErrorMatchKind::Pattern { pattern, .. } => pattern.span(),
                        ErrorMatchKind::Code(code) => code.span(),
                    };
                    parser.errors.push(Error::InvalidComment {
                        msg: format!(
                            "//~v pattern is trying to refer to line {}, but the file only has {} lines",
                            m.line.get(),
                            last_line,
                        ),
                        span,
                    });
                }
            }
        }

        for (span, ..) in delayed_fallthrough {
            parser.error(span, "`//~|` comment not attached to anchoring matcher");
        }

        let Revisioned {
            span,
            ignore,
            only,
            stderr_per_bitwidth,
            compile_flags,
            env_vars,
            normalize_stderr,
            normalize_stdout,
            error_in_other_files,
            error_matches,
            require_annotations_for_level,
            aux_builds,
            edition,
            mode,
            needs_asm_support,
            no_rustfix,
            diagnostic_code_prefix,
        } = parser.comments.base();
        if span.is_dummy() {
            *span = defaults.span;
        }
        ignore.extend(defaults.ignore);
        only.extend(defaults.only);
        *stderr_per_bitwidth |= defaults.stderr_per_bitwidth;
        compile_flags.extend(defaults.compile_flags);
        env_vars.extend(defaults.env_vars);
        normalize_stderr.extend(defaults.normalize_stderr);
        normalize_stdout.extend(defaults.normalize_stdout);
        error_in_other_files.extend(defaults.error_in_other_files);
        error_matches.extend(defaults.error_matches);
        aux_builds.extend(defaults.aux_builds);
        if require_annotations_for_level.is_none() {
            *require_annotations_for_level = defaults.require_annotations_for_level;
        }
        if edition.is_none() {
            *edition = defaults.edition;
        }
        if mode.is_none() {
            *mode = defaults.mode;
        }
        if no_rustfix.is_none() {
            *no_rustfix = defaults.no_rustfix;
        }
        if diagnostic_code_prefix.is_none() {
            *diagnostic_code_prefix = defaults.diagnostic_code_prefix;
        }
        *needs_asm_support |= defaults.needs_asm_support;

        if parser.errors.is_empty() {
            Ok(parser.comments)
        } else {
            Err(parser.errors)
        }
    }
}

impl CommentParser<Comments> {
    fn parse_checked_line(
        &mut self,
        fallthrough_to: Option<NonZeroUsize>,
        line: Spanned<&[u8]>,
    ) -> std::result::Result<ParsePatternResult, Spanned<Utf8Error>> {
        let mut res = ParsePatternResult::Other;
        if let Some(command) = line.strip_prefix(b"//@") {
            self.parse_command(command.to_str()?.trim())
        } else if let Some((_, pattern)) = line.split_once_str("//~") {
            let (revisions, pattern) = self.parse_revisions(pattern.to_str()?);
            self.revisioned(revisions, |this| {
                res = this.parse_pattern(pattern, fallthrough_to);
            })
        } else {
            for pos in line.clone().find_iter("//") {
                let (_, rest) = line.clone().to_str()?.split_at(pos + 2);
                for rest in std::iter::once(rest.clone()).chain(rest.strip_prefix(" ")) {
                    let c = rest.chars().next();
                    if let Some(Spanned {
                        content: '@' | '~' | '[' | ']' | '^' | '|',
                        span,
                    }) = c
                    {
                        self.error(
                            span,
                            format!(
                                "comment looks suspiciously like a test suite command: `{}`\n\
                             All `//@` test suite commands must be at the start of the line.\n\
                             The `//` must be directly followed by `@` or `~`.",
                                *rest,
                            ),
                        );
                    } else {
                        let mut parser = Self {
                            errors: vec![],
                            comments: Comments::default(),
                            commands: std::mem::take(&mut self.commands),
                        };
                        let span = rest.span();
                        parser.parse_command(rest);
                        if parser.errors.is_empty() {
                            self.error(
                                span,
                                "a compiletest-rs style comment was detected.\n\
                                Please use text that could not also be interpreted as a command,\n\
                                and prefix all actual commands with `//@`",
                            );
                        }
                        self.commands = parser.commands;
                    }
                }
            }
        }
        Ok(res)
    }
}

impl<CommentsType> CommentParser<CommentsType> {
    fn error(&mut self, span: Span, s: impl Into<String>) {
        self.errors.push(Error::InvalidComment {
            msg: s.into(),
            span,
        });
    }

    fn check(&mut self, span: Span, cond: bool, s: impl Into<String>) {
        if !cond {
            self.error(span, s);
        }
    }

    fn check_some<T>(&mut self, span: Span, opt: Option<T>, s: impl Into<String>) -> Option<T> {
        self.check(span, opt.is_some(), s);
        opt
    }
}

impl CommentParser<Comments> {
    fn parse_command(&mut self, command: Spanned<&str>) {
        let (revisions, command) = self.parse_revisions(command);

        // Commands are letters or dashes, grab everything until the first character that is neither of those.
        let (command, args) = match command
            .char_indices()
            .find_map(|(i, c)| (!c.is_alphanumeric() && c != '-' && c != '_').then_some(i))
        {
            None => {
                let span = command.span().shrink_to_end();
                (command, Spanned::new("", span))
            }
            Some(i) => {
                let (command, args) = command.split_at(i);
                // Commands are separated from their arguments by ':' or ' '
                let next = args
                    .chars()
                    .next()
                    .expect("the `position` above guarantees that there is at least one char");
                let pos = next.len_utf8();
                self.check(
                    next.span,
                    next.content == ':',
                    "test command must be followed by `:` (or end the line)",
                );
                (command, args.split_at(pos).1.trim())
            }
        };

        if *command == "revisions" {
            self.check(
                revisions.span(),
                revisions.is_empty(),
                "revisions cannot be declared under a revision",
            );
            self.check(
                revisions.span(),
                self.revisions.is_none(),
                "cannot specify `revisions` twice",
            );
            self.revisions = Some(args.split_whitespace().map(|s| s.to_string()).collect());
            return;
        }
        self.revisioned(revisions, |this| this.parse_command(command, args));
    }

    fn revisioned(
        &mut self,
        revisions: Spanned<Vec<String>>,
        f: impl FnOnce(&mut CommentParser<&mut Revisioned>),
    ) {
        let Spanned {
            content: revisions,
            span,
        } = revisions;
        let mut this = CommentParser {
            errors: std::mem::take(&mut self.errors),
            commands: std::mem::take(&mut self.commands),
            comments: self
                .revisioned
                .entry(revisions)
                .or_insert_with(|| Revisioned {
                    span,
                    ..Default::default()
                }),
        };
        f(&mut this);
        let CommentParser {
            errors, commands, ..
        } = this;
        self.commands = commands;
        self.errors = errors;
    }
}

impl CommentParser<&mut Revisioned> {
    fn parse_normalize_test(
        &mut self,
        args: Spanned<&str>,
        mode: &str,
    ) -> Option<(Regex, Vec<u8>)> {
        let (from, rest) = self.parse_str(args);

        let to = match rest.strip_prefix("->") {
            Some(v) => v,
            None => {
                self.error(
                    rest.span(),
                    format!(
                        "normalize-{mode}-test needs a pattern and replacement separated by `->`"
                    ),
                );
                return None;
            }
        }
        .trim_start();
        let (to, rest) = self.parse_str(to);

        self.check(
            rest.span(),
            rest.is_empty(),
            "trailing text after pattern replacement",
        );

        let regex = self.parse_regex(from)?.content;
        Some((regex, to.as_bytes().to_owned()))
    }

    fn commands() -> HashMap<&'static str, CommandParserFunc> {
        let mut commands = HashMap::<_, CommandParserFunc>::new();
        macro_rules! commands {
            ($($name:expr => ($this:ident, $args:ident, $span:ident)$block:block)*) => {
                $(commands.insert($name, |$this, $args, $span| {
                    $block
                });)*
            };
        }
        commands! {
            "compile-flags" => (this, args, _span){
                if let Some(parsed) = comma::parse_command(*args) {
                    this.compile_flags.extend(parsed);
                } else {
                    this.error(args.span(), format!("`{}` contains an unclosed quotation mark", *args));
                }
            }
            "rustc-env" => (this, args, _span){
                for env in args.split_whitespace() {
                    if let Some((k, v)) = this.check_some(
                        args.span(),
                        env.split_once('='),
                        "environment variables must be key/value pairs separated by a `=`",
                    ) {
                        this.env_vars.push((k.to_string(), v.to_string()));
                    }
                }
            }
            "normalize-stderr-test" => (this, args, _span){
                if let Some((regex, replacement)) = this.parse_normalize_test(args, "stderr") {
                    this.normalize_stderr.push((regex.into(), replacement))
                }
            }
            "normalize-stdout-test" => (this, args, _span){
                if let Some((regex, replacement)) = this.parse_normalize_test(args, "stdout") {
                    this.normalize_stdout.push((regex.into(), replacement))
                }
            }
            "error-pattern" => (this, _args, span){
                this.error(span, "`error-pattern` has been renamed to `error-in-other-file`");
            }
            "error-in-other-file" => (this, args, _span){
                let args = args.trim();
                let pat = this.parse_error_pattern(args);
                this.error_in_other_files.push(pat);
            }
            "stderr-per-bitwidth" => (this, _args, span){
                // args are ignored (can be used as comment)
                this.check(
                    span,
                    !this.stderr_per_bitwidth,
                    "cannot specify `stderr-per-bitwidth` twice",
                );
                this.stderr_per_bitwidth = true;
            }
            "run-rustfix" => (this, _args, span){
                this.error(span, "rustfix is now ran by default when applicable suggestions are found");
            }
            "no-rustfix" => (this, _args, span){
                // args are ignored (can be used as comment)
                let prev = this.no_rustfix.set((), span.clone());
                this.check(
                    span,
                    prev.is_none(),
                    "cannot specify `no-rustfix` twice",
                );
            }
            "needs-asm-support" => (this, _args, span){
                // args are ignored (can be used as comment)
                this.check(
                    span,
                    !this.needs_asm_support,
                    "cannot specify `needs-asm-support` twice",
                );
                this.needs_asm_support = true;
            }
            "aux-build" => (this, args, _span){
                let name = match args.split_once(":") {
                    Some((name, rest)) => {
                        this.error(rest.span(), "proc macros are now auto-detected, you can remove the `:proc-macro` after the file name");
                        name
                    },
                    None => args,
                };
                this.aux_builds.push(name.map(Into::into));
            }
            "edition" => (this, args, span){
                let prev = this.edition.set((*args).into(), args.span());
                this.check(span, prev.is_none(), "cannot specify `edition` twice");
            }
            "check-pass" => (this, _args, span){
                let prev = this.mode.set(Mode::Pass, span.clone());
                // args are ignored (can be used as comment)
                this.check(
                    span,
                    prev.is_none(),
                    "cannot specify test mode changes twice",
                );
            }
            "run" => (this, args, span){
                this.check(
                    span,
                    this.mode.is_none(),
                    "cannot specify test mode changes twice",
                );
                let mut set = |exit_code| this.mode.set(Mode::Run { exit_code }, args.span());
                if args.is_empty() {
                    set(0);
                } else {
                    match args.content.parse() {
                        Ok(exit_code) => {set(exit_code);},
                        Err(err) => this.error(args.span(), err.to_string()),
                    }
                }
            }
            "require-annotations-for-level" => (this, args, span){
                let args = args.trim();
                let prev = match args.content.parse() {
                    Ok(it) =>  this.require_annotations_for_level.set(it, args.span()),
                    Err(msg) => {
                        this.error(args.span(), msg);
                        None
                    },
                };

                this.check(
                    span,
                    prev.is_none(),
                    "cannot specify `require-annotations-for-level` twice",
                );
            }
        }
        commands
    }

    fn parse_command(&mut self, command: Spanned<&str>, args: Spanned<&str>) {
        if let Some(command_handler) = self.commands.get(*command) {
            command_handler(self, args, command.span());
        } else if let Some(s) = command.strip_prefix("ignore-") {
            // args are ignored (can be used as comment)
            match Condition::parse(*s) {
                Ok(cond) => self.ignore.push(cond),
                Err(msg) => self.error(s.span(), msg),
            }
        } else if let Some(s) = command.strip_prefix("only-") {
            // args are ignored (can be used as comment)
            match Condition::parse(*s) {
                Ok(cond) => self.only.push(cond),
                Err(msg) => self.error(s.span(), msg),
            }
        } else {
            let best_match = self
                .commands
                .keys()
                .min_by_key(|key| levenshtein::levenshtein(key, *command))
                .unwrap();
            self.error(
                command.span(),
                format!(
                    "`{}` is not a command known to `ui_test`, did you mean `{best_match}`?",
                    *command
                ),
            );
        }
    }
}

impl<CommentsType> CommentParser<CommentsType> {
    fn parse_regex(&mut self, regex: Spanned<&str>) -> Option<Spanned<Regex>> {
        match Regex::new(*regex) {
            Ok(r) => Some(regex.map(|_| r)),
            Err(err) => {
                self.error(regex.span(), format!("invalid regex: {err:?}"));
                None
            }
        }
    }

    /// Parses a string literal. `s` has to start with `"`; everything until the next `"` is
    /// returned in the first component. `\` can be used to escape arbitrary character.
    /// Second return component is the rest of the string with leading whitespace removed.
    fn parse_str<'a>(&mut self, s: Spanned<&'a str>) -> (Spanned<&'a str>, Spanned<&'a str>) {
        match s.strip_prefix("\"") {
            Some(s) => {
                let mut escaped = false;
                for (i, c) in s.char_indices() {
                    if escaped {
                        // Accept any character as literal after a `\`.
                        escaped = false;
                    } else if c == '"' {
                        let (a, b) = s.split_at(i);
                        let b = b.split_at(1).1;
                        return (a, b.trim_start());
                    } else {
                        escaped = c == '\\';
                    }
                }
                self.error(s.span(), format!("no closing quotes found for {}", *s));
                let span = s.span();
                (s, Spanned::new("", span))
            }
            None => {
                if s.is_empty() {
                    self.error(s.span(), "expected quoted string, but found end of line")
                } else {
                    let c = s.chars().next().unwrap();
                    self.error(c.span, format!("expected `\"`, got `{}`", c.content))
                }
                let span = s.span();
                (s, Spanned::new("", span))
            }
        }
    }

    // parse something like \[[a-z]+(,[a-z]+)*\]
    fn parse_revisions<'a>(
        &mut self,
        pattern: Spanned<&'a str>,
    ) -> (Spanned<Vec<String>>, Spanned<&'a str>) {
        match pattern.strip_prefix("[") {
            Some(s) => {
                // revisions
                let end = s.char_indices().find_map(|(i, c)| match c {
                    ']' => Some(i),
                    _ => None,
                });
                let Some(end) = end else {
                    self.error(s.span(), "`[` without corresponding `]`");
                    return (
                        Spanned::new(vec![], pattern.span().shrink_to_start()),
                        pattern,
                    );
                };
                let (revision, pattern) = s.split_at(end);
                let revisions = revision.split(',').map(|s| s.trim().to_string()).collect();
                (
                    Spanned::new(revisions, revision.span()),
                    // 1.. because `split_at` includes the separator
                    pattern.split_at(1).1.trim_start(),
                )
            }
            _ => (
                Spanned::new(vec![], pattern.span().shrink_to_start()),
                pattern,
            ),
        }
    }
}

impl CommentParser<&mut Revisioned> {
    // parse something like:
    // (\[[a-z]+(,[a-z]+)*\])?
    // (?P<offset>\||[\^]+)? *
    // ((?P<level>ERROR|HELP|WARN|NOTE): (?P<text>.*))|(?P<code>[a-z0-9_:]+)
    fn parse_pattern(
        &mut self,
        pattern: Spanned<&str>,
        fallthrough_to: Option<NonZeroUsize>,
    ) -> ParsePatternResult {
        let c = pattern.chars().next();
        let mut res = ParsePatternResult::Other;

        let (match_line, pattern) = match c {
            Some(Spanned { content: '|', span }) => (
                match fallthrough_to {
                    Some(match_line) => {
                        res = ParsePatternResult::ErrorAbove { match_line };
                        match_line
                    }
                    None => {
                        res = ParsePatternResult::Fallthrough {
                            span,
                            idx: self.error_matches.len(),
                        };
                        pattern.span.line_start
                    }
                },
                pattern.split_at(1).1,
            ),
            Some(Spanned {
                content: '^',
                span: _,
            }) => {
                let offset = pattern.chars().take_while(|c| c.content == '^').count();
                match pattern
                    .span()
                    .line_start
                    .get()
                    .checked_sub(offset)
                    .and_then(NonZeroUsize::new)
                {
                    // lines are one-indexed, so a target line of 0 is invalid, but also
                    // prevented via `NonZeroUsize`
                    Some(match_line) => {
                        res = ParsePatternResult::ErrorAbove { match_line };
                        (match_line, pattern.split_at(offset).1)
                    }
                    _ => {
                        self.error(pattern.span(), format!(
                            "//~^ pattern is trying to refer to {} lines above, but there are only {} lines above",
                            offset,
                            pattern.line().get() - 1,
                        ));
                        return ParsePatternResult::ErrorAbove {
                            match_line: pattern.span().line_start,
                        };
                    }
                }
            }
            Some(Spanned {
                content: 'v',
                span: _,
            }) => {
                let offset = pattern.chars().take_while(|c| c.content == 'v').count();
                match pattern
                    .span()
                    .line_start
                    .get()
                    .checked_add(offset)
                    .and_then(NonZeroUsize::new)
                {
                    Some(match_line) => {
                        res = ParsePatternResult::ErrorBelow {
                            span: pattern.span(),
                            match_line,
                        };
                        (match_line, pattern.split_at(offset).1)
                    }
                    _ => {
                        // The line count of the file is not yet known so we can only check
                        // if the resulting line is in the range of a usize.
                        self.error(pattern.span(), format!(
                            "//~v pattern is trying to refer to {} lines below, which is more than ui_test can count",
                            offset,
                        ));
                        return ParsePatternResult::ErrorBelow {
                            span: pattern.span(),
                            match_line: pattern.span().line_start,
                        };
                    }
                }
            }
            Some(_) => (pattern.span().line_start, pattern),
            None => {
                self.error(pattern.span(), "no pattern specified");
                return res;
            }
        };

        let pattern = pattern.trim_start();
        let offset = pattern
            .bytes()
            .position(|c| !(c.is_ascii_alphanumeric() || c == b'_' || c == b':'))
            .unwrap_or(pattern.len());

        let (level_or_code, pattern) = pattern.split_at(offset);
        if let Some(level) = level_or_code.strip_suffix(":") {
            let level = match (*level).parse() {
                Ok(level) => level,
                Err(msg) => {
                    self.error(level.span(), msg);
                    return res;
                }
            };

            let pattern = pattern.trim();

            self.check(pattern.span(), !pattern.is_empty(), "no pattern specified");

            let pattern = self.parse_error_pattern(pattern);

            self.error_matches.push(ErrorMatch {
                kind: ErrorMatchKind::Pattern { pattern, level },
                line: match_line,
            });
        } else if (*level_or_code).parse::<Level>().is_ok() {
            // Shouldn't conflict with any real diagnostic code
            self.error(level_or_code.span(), "no `:` after level found");
            return res;
        } else if !pattern.trim_start().is_empty() {
            self.error(
                pattern.span(),
                format!("text found after error code `{}`", *level_or_code),
            );
            return res;
        } else {
            self.error_matches.push(ErrorMatch {
                kind: ErrorMatchKind::Code(Spanned::new(
                    level_or_code.to_string(),
                    level_or_code.span(),
                )),
                line: match_line,
            });
        };

        res
    }
}

impl Pattern {
    pub(crate) fn matches(&self, message: &str) -> bool {
        match self {
            Pattern::SubString(s) => message.contains(s),
            Pattern::Regex(r) => r.is_match(message.as_bytes()),
        }
    }
}

impl<CommentsType> CommentParser<CommentsType> {
    fn parse_error_pattern(&mut self, pattern: Spanned<&str>) -> Spanned<Pattern> {
        if let Some(regex) = pattern.strip_prefix("/") {
            match regex.strip_suffix("/") {
                Some(regex) => match self.parse_regex(regex) {
                    Some(r) => r.map(Pattern::Regex),
                    None => pattern.map(|p| Pattern::SubString(p.to_string())),
                },
                None => {
                    self.error(
                        regex.span(),
                        "expected regex pattern due to leading `/`, but found no closing `/`",
                    );
                    pattern.map(|p| Pattern::SubString(p.to_string()))
                }
            }
        } else {
            pattern.map(|p| Pattern::SubString(p.to_string()))
        }
    }
}
