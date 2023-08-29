use std::{
    collections::HashMap,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    process::Command,
};

use bstr::{ByteSlice, Utf8Error};
use regex::bytes::Regex;

use crate::{
    rustc_stderr::{Level, Span},
    Error, Errored, Mode,
};

use color_eyre::eyre::{Context, Result};

pub(crate) use spanned::*;

mod spanned;
#[cfg(test)]
mod tests;

/// This crate supports various magic comments that get parsed as file-specific
/// configuration values. This struct parses them all in one go and then they
/// get processed by their respective use sites.
#[derive(Default, Debug)]
pub(crate) struct Comments {
    /// List of revision names to execute. Can only be specified once
    pub revisions: Option<Vec<String>>,
    /// Comments that are only available under specific revisions.
    /// The defaults are in key `vec![]`
    pub revisioned: HashMap<Vec<String>, Revisioned>,
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
        for rev in self.for_revision(revision) {
            if let Some(found) = f(rev).into_inner() {
                if result.is_some() {
                    errors.push(found.line());
                } else {
                    result = found.into();
                }
            }
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

    pub(crate) fn edition(
        &self,
        revision: &str,
        config: &crate::Config,
    ) -> Result<Option<MaybeSpanned<String>>, Errored> {
        let edition =
            self.find_one_for_revision(revision, "`edition` annotations", |r| r.edition.clone())?;
        let edition = edition
            .into_inner()
            .map(MaybeSpanned::from)
            .or(config.edition.clone().map(MaybeSpanned::new_config));
        Ok(edition)
    }
}

#[derive(Debug)]
/// Comments that can be filtered for specific revisions.
pub(crate) struct Revisioned {
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
    pub normalize_stderr: Vec<(Regex, Vec<u8>)>,
    /// Normalizations to apply to the stdout output before emitting it to disk
    pub normalize_stdout: Vec<(Regex, Vec<u8>)>,
    /// Arbitrary patterns to look for in the stderr.
    /// The error must be from another file, as errors from the current file must be
    /// checked via `error_matches`.
    pub error_in_other_files: Vec<Spanned<Pattern>>,
    pub error_matches: Vec<ErrorMatch>,
    /// Ignore diagnostics below this level.
    /// `None` means pick the lowest level from the `error_pattern`s.
    pub require_annotations_for_level: OptWithLine<Level>,
    pub aux_builds: Vec<Spanned<PathBuf>>,
    pub edition: OptWithLine<String>,
    /// Overwrites the mode from `Config`.
    pub mode: OptWithLine<Mode>,
    pub needs_asm_support: bool,
    /// Don't run [`rustfix`] for this test
    pub no_rustfix: OptWithLine<()>,
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
#[derive(Debug)]
pub(crate) enum Condition {
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

#[derive(Debug)]
pub(crate) struct ErrorMatch {
    pub pattern: Spanned<Pattern>,
    pub level: Level,
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

impl Comments {
    pub(crate) fn parse_file(path: &Path) -> Result<std::result::Result<Self, Vec<Error>>> {
        let content =
            std::fs::read(path).wrap_err_with(|| format!("failed to read {}", path.display()))?;
        Ok(Self::parse(&content))
    }

    /// Parse comments in `content`.
    /// `path` is only used to emit diagnostics if parsing fails.
    pub(crate) fn parse(
        content: &(impl AsRef<[u8]> + ?Sized),
    ) -> std::result::Result<Self, Vec<Error>> {
        let mut parser = CommentParser {
            comments: Comments::default(),
            errors: vec![],
            commands: CommentParser::<_>::commands(),
        };

        let mut fallthrough_to = None; // The line that a `|` will refer to.
        for (l, line) in content.as_ref().lines().enumerate() {
            let l = NonZeroUsize::new(l + 1).unwrap(); // enumerate starts at 0, but line numbers start at 1
            let span = Span {
                line_start: l,
                line_end: l,
                column_start: NonZeroUsize::new(1).unwrap(),
                column_end: NonZeroUsize::new(line.chars().count() + 1).unwrap(),
            };
            match parser.parse_checked_line(&mut fallthrough_to, Spanned::new(line, span)) {
                Ok(()) => {}
                Err(e) => parser.error(span, format!("Comment is not utf8: {e:?}")),
            }
        }
        if let Some(revisions) = &parser.comments.revisions {
            for (key, revisioned) in &parser.comments.revisioned {
                for rev in key {
                    if !revisions.contains(rev) {
                        parser.errors.push(Error::InvalidComment {
                            msg: format!("the revision `{rev}` is not known"),
                            span: revisioned.span,
                        })
                    }
                }
            }
        } else {
            for (key, revisioned) in &parser.comments.revisioned {
                if !key.is_empty() {
                    parser.errors.push(Error::InvalidComment {
                        msg: "there are no revisions in this test".into(),
                        span: revisioned.span,
                    })
                }
            }
        }
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
        fallthrough_to: &mut Option<NonZeroUsize>,
        line: Spanned<&[u8]>,
    ) -> std::result::Result<(), Utf8Error> {
        if let Some(command) = line.strip_prefix(b"//@") {
            self.parse_command(command.to_str()?.trim())
        } else if let Some((_, pattern)) = line.split_once_str("//~") {
            let (revisions, pattern) = self.parse_revisions(pattern.to_str()?);
            self.revisioned(revisions, |this| {
                this.parse_pattern(pattern, fallthrough_to)
            })
        } else {
            *fallthrough_to = None;
            for pos in line.find_iter("//") {
                let (_, rest) = line.to_str()?.split_at(pos + 2);
                for rest in std::iter::once(rest).chain(rest.strip_prefix(" ")) {
                    if let Some('@' | '~' | '[' | ']' | '^' | '|') = rest.chars().next() {
                        self.error(
                            rest.span(),
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
                        parser.parse_command(rest);
                        if parser.errors.is_empty() {
                            self.error(
                                rest.span(),
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
        Ok(())
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
            None => (command, Spanned::new("", command.span().shrink_to_end())),
            Some(i) => {
                let (command, args) = command.split_at(i);
                // Commands are separated from their arguments by ':' or ' '
                let next = args
                    .chars()
                    .next()
                    .expect("the `position` above guarantees that there is at least one char");
                self.check(
                    args.span().shrink_to_start(),
                    next == ':',
                    "test command must be followed by `:` (or end the line)",
                );
                (command, args.split_at(next.len_utf8()).1.trim())
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
        let span = revisions.span();
        let revisions = revisions.into_inner();
        let mut this = CommentParser {
            errors: std::mem::take(&mut self.errors),
            commands: std::mem::take(&mut self.commands),
            comments: self
                .revisioned
                .entry(revisions)
                .or_insert_with(|| Revisioned {
                    span,
                    ignore: Default::default(),
                    only: Default::default(),
                    stderr_per_bitwidth: Default::default(),
                    compile_flags: Default::default(),
                    env_vars: Default::default(),
                    normalize_stderr: Default::default(),
                    normalize_stdout: Default::default(),
                    error_in_other_files: Default::default(),
                    error_matches: Default::default(),
                    require_annotations_for_level: Default::default(),
                    aux_builds: Default::default(),
                    edition: Default::default(),
                    mode: Default::default(),
                    needs_asm_support: Default::default(),
                    no_rustfix: Default::default(),
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

        let regex = self.parse_regex(from)?;
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
                if let Some(res) = this.parse_normalize_test(args, "stderr") {
                    this.normalize_stderr.push(res)
                }
            }
            "normalize-stdout-test" => (this, args, _span){
                if let Some(res) = this.parse_normalize_test(args, "stdout") {
                    this.normalize_stdout.push(res)
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
                let prev = this.no_rustfix.set((), span);
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
                let prev = this.mode.set(Mode::Pass, span);
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
                    match args.parse() {
                        Ok(exit_code) => {set(exit_code);},
                        Err(err) => this.error(args.span(), err.to_string()),
                    }
                }
            }
            "require-annotations-for-level" => (this, args, span){
                let args = args.trim();
                let prev = match args.parse() {
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
    fn parse_regex(&mut self, regex: Spanned<&str>) -> Option<Regex> {
        match Regex::new(*regex) {
            Ok(regex) => Some(regex),
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
                (s, Spanned::new("", s.span()))
            }
            None => {
                if s.is_empty() {
                    self.error(s.span(), "expected quoted string, but found end of line")
                } else {
                    self.error(
                        s.span(),
                        format!("expected `\"`, got `{}`", s.chars().next().unwrap()),
                    )
                }
                (s, Spanned::new("", s.span()))
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
    // parse something like (\[[a-z]+(,[a-z]+)*\])?(?P<offset>\||[\^]+)? *(?P<level>ERROR|HELP|WARN|NOTE): (?P<text>.*)
    fn parse_pattern(&mut self, pattern: Spanned<&str>, fallthrough_to: &mut Option<NonZeroUsize>) {
        let (match_line, pattern) = match pattern.chars().next() {
            Some('|') => (
                match fallthrough_to {
                    Some(fallthrough) => *fallthrough,
                    None => {
                        self.error(pattern.span(), "`//~|` pattern without preceding line");
                        return;
                    }
                },
                pattern.split_at(1).1,
            ),
            Some('^') => {
                let offset = pattern.chars().take_while(|&c| c == '^').count();
                match pattern
                    .span()
                    .line_start
                    .get()
                    .checked_sub(offset)
                    .and_then(NonZeroUsize::new)
                {
                    // lines are one-indexed, so a target line of 0 is invalid, but also
                    // prevented via `NonZeroUsize`
                    Some(match_line) => (match_line, pattern.split_at(offset).1),
                    _ => {
                        self.error(pattern.span(), format!(
                            "//~^ pattern is trying to refer to {} lines above, but there are only {} lines above",
                            offset,
                            pattern.line().get() - 1,
                        ));
                        return;
                    }
                }
            }
            Some(_) => (pattern.span().line_start, pattern),
            None => {
                self.error(pattern.span(), "no pattern specified");
                return;
            }
        };

        let pattern = pattern.trim_start();
        let offset = match pattern.chars().position(|c| !c.is_ascii_alphabetic()) {
            Some(offset) => offset,
            None => {
                self.error(pattern.span(), "pattern without level");
                return;
            }
        };

        let (level, pattern) = pattern.split_at(offset);
        let level = match (*level).parse() {
            Ok(level) => level,
            Err(msg) => {
                self.error(level.span(), msg);
                return;
            }
        };
        let pattern = match pattern.strip_prefix(":") {
            Some(offset) => offset,
            None => {
                self.error(pattern.span(), "no `:` after level found");
                return;
            }
        };

        let pattern = pattern.trim();

        self.check(pattern.span(), !pattern.is_empty(), "no pattern specified");

        let pattern = self.parse_error_pattern(pattern);

        *fallthrough_to = Some(match_line);

        self.error_matches.push(ErrorMatch {
            pattern,
            level,
            line: match_line,
        });
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
                    Some(r) => Spanned::new(Pattern::Regex(r), regex.span()),
                    None => Spanned::new(Pattern::SubString(pattern.to_string()), regex.span()),
                },
                None => {
                    self.error(
                        regex.span(),
                        "expected regex pattern due to leading `/`, but found no closing `/`",
                    );
                    Spanned::new(Pattern::SubString(pattern.to_string()), regex.span())
                }
            }
        } else {
            Spanned::new(Pattern::SubString(pattern.to_string()), pattern.span())
        }
    }
}
