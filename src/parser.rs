use std::path::Path;

use bstr::{ByteSlice, Utf8Error};
use regex::bytes::Regex;

use crate::{rustc_stderr::Level, Error};

use color_eyre::eyre::Result;

#[cfg(test)]
mod tests;

/// This crate supports various magic comments that get parsed as file-specific
/// configuration values. This struct parses them all in one go and then they
/// get processed by their respective use sites.
#[derive(Default, Debug)]
pub(crate) struct Comments {
    /// List of revision names to execute. Can only be speicified once
    pub revisions: Option<Vec<String>>,
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
    /// An arbitrary pattern to look for in the stderr.
    pub error_pattern: Option<(Pattern, usize)>,
    pub error_matches: Vec<ErrorMatch>,
    /// Ignore diagnostics below this level.
    /// `None` means pick the lowest level from the `error_pattern`s.
    pub require_annotations_for_level: Option<Level>,
}

#[derive(Default, Debug)]
struct CommentParser {
    /// The comments being built.
    comments: Comments,
    /// Any errors that ocurred during comment parsing.
    errors: Vec<Error>,
    /// The line currently being parsed.
    line: usize,
}

impl std::ops::Deref for CommentParser {
    type Target = Comments;

    fn deref(&self) -> &Self::Target {
        &self.comments
    }
}

impl std::ops::DerefMut for CommentParser {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.comments
    }
}

/// The conditions used for "ignore" and "only" filters.
#[derive(Debug)]
pub(crate) enum Condition {
    /// The given string must appear in the target.
    Target(String),
    /// Tests that the bitwidth is the given one.
    Bitwidth(u8),
    /// Tests that the target is the host.
    OnHost,
}

#[derive(Debug, Clone)]
pub(crate) enum Pattern {
    SubString(String),
    Regex(Regex),
}

#[derive(Debug)]
pub(crate) struct ErrorMatch {
    pub pattern: Pattern,
    pub revision: Option<String>,
    pub level: Level,
    /// The line where the message was defined, for reporting issues with it (e.g. in case it wasn't found).
    pub definition_line: usize,
    /// The line this pattern is expecting to find a message in.
    pub line: usize,
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
        } else if let Some(target) = c.strip_prefix("target-") {
            Ok(Condition::Target(target.to_owned()))
        } else {
            Err(format!(
                "invalid condition `{c:?}`, expected `on-host`, /[0-9]+bit/ or /target-.*/"
            ))
        }
    }
}

impl Comments {
    pub(crate) fn parse_file(path: &Path) -> Result<std::result::Result<Self, Vec<Error>>> {
        let content = std::fs::read(path)?;
        Ok(Self::parse(&content))
    }

    /// Parse comments in `content`.
    /// `path` is only used to emit diagnostics if parsing fails.
    pub(crate) fn parse(
        content: &(impl AsRef<[u8]> + ?Sized),
    ) -> std::result::Result<Self, Vec<Error>> {
        let mut parser = CommentParser::default();

        let mut fallthrough_to = None; // The line that a `|` will refer to.
        for (l, line) in content.as_ref().lines().enumerate() {
            let l = l + 1; // enumerate starts at 0, but line numbers start at 1
            parser.line = l;
            match parser.parse_checked_line(&mut fallthrough_to, line) {
                Ok(()) => {}
                Err(e) => parser.errors.push(Error::InvalidComment {
                    msg: format!("Comment is not utf8: {e:?}"),
                    line: l,
                }),
            }
        }
        if parser.errors.is_empty() {
            Ok(parser.comments)
        } else {
            Err(parser.errors)
        }
    }
}

impl CommentParser {
    fn parse_checked_line(
        &mut self,
        fallthrough_to: &mut Option<usize>,
        line: &[u8],
    ) -> std::result::Result<(), Utf8Error> {
        Ok(if let Some((_, command)) = line.split_once_str("//@") {
            self.parse_command(command.trim().to_str()?)
        } else if let Some((_, pattern)) = line.split_once_str("//~") {
            self.parse_pattern(pattern.to_str()?, fallthrough_to)
        } else if let Some((_, pattern)) = line.split_once_str("//[") {
            self.parse_revisioned_pattern(pattern.to_str()?, fallthrough_to)
        } else {
            *fallthrough_to = None;
        })
    }

    fn error(&mut self, s: impl Into<String>) {
        self.errors.push(Error::InvalidComment {
            msg: s.into(),
            line: self.line,
        });
    }

    fn check(&mut self, cond: bool, s: impl Into<String>) {
        if !cond {
            self.error(s);
        }
    }

    fn check_some<T>(&mut self, opt: Option<T>, s: impl Into<String>) -> Option<T> {
        self.check(opt.is_some(), s);
        opt
    }

    fn parse_command(&mut self, command: &str) {
        // Commands are letters or dashes, grab everything until the first character that is neither of those.
        let (command, args) = match command
            .chars()
            .position(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
        {
            None => (command, ""),
            Some(i) => {
                let (command, args) = command.split_at(i);
                let mut args = args.chars();
                // Commands are separated from their arguments by ':' or ' '
                let next = args
                    .next()
                    .expect("the `position` above guarantees that there is at least one char");
                self.check(
                    next == ':',
                    "test command must be followed by `:` (or end the line)",
                );
                (command, args.as_str().trim())
            }
        };

        match command {
            "revisions" => {
                self.check(self.revisions.is_none(), "cannot specify `revisions` twice");
                self.revisions = Some(args.split_whitespace().map(|s| s.to_string()).collect());
            }
            "compile-flags" => {
                self.compile_flags
                    .extend(args.split_whitespace().map(|s| s.to_string()));
            }
            "rustc-env" => {
                for env in args.split_whitespace() {
                    if let Some((k, v)) = self.check_some(
                        env.split_once('='),
                        "environment variables must be key/value pairs separated by a `=`",
                    ) {
                        self.env_vars.push((k.to_string(), v.to_string()));
                    }
                }
            }
            "normalize-stderr-test" => {
                let (from, rest) = self.parse_str(args);

                let to = match rest.strip_prefix("->") {
                    Some(v) => v,
                    None => {
                        self.error("normalize-stderr-test needs a pattern and replacement separated by `->`");
                        return;
                    },
                }.trim_start();
                let (to, rest) = self.parse_str(to);

                self.check(
                    rest.is_empty(),
                    format!("trailing text after pattern replacement: {rest}"),
                );

                if let Some(regex) = self.parse_regex(from) {
                    self.normalize_stderr
                        .push((regex, to.as_bytes().to_owned()))
                }
            }
            "error-pattern" => {
                self.check(
                    self.error_pattern.is_none(),
                    "cannot specify `error_pattern` twice",
                );
                self.error_pattern = Some((self.parse_error_pattern(args.trim()), self.line))
            }
            "stderr-per-bitwidth" => {
                // args are ignored (can be used as comment)
                self.check(
                    !self.stderr_per_bitwidth,
                    "cannot specify `stderr-per-bitwidth` twice",
                );
                self.stderr_per_bitwidth = true;
            }
            "require-annotations-for-level" => {
                self.check(
                    self.require_annotations_for_level.is_none(),
                    "cannot specify `require-annotations-for-level` twice",
                );
                match args.trim().parse() {
                    Ok(it) => self.require_annotations_for_level = Some(it),
                    Err(msg) => self.error(msg),
                }
            }
            command => {
                if let Some(s) = command.strip_prefix("ignore-") {
                    // args are ignored (can be used as comment)
                    match Condition::parse(s) {
                        Ok(cond) => self.ignore.push(cond),
                        Err(msg) => self.error(msg),
                    }
                    return;
                }

                if let Some(s) = command.strip_prefix("only-") {
                    // args are ignored (can be used as comment)
                    match Condition::parse(s) {
                        Ok(cond) => self.only.push(cond),
                        Err(msg) => self.error(msg),
                    }
                    return;
                }
                self.error(format!("unknown command `{command}`"));
            }
        }
    }

    fn parse_regex(&mut self, regex: &str) -> Option<Regex> {
        match Regex::new(regex) {
            Ok(regex) => Some(regex),
            Err(err) => {
                self.error(format!("invalid regex: {err:?}"));
                None
            }
        }
    }

    /// Parses a string literal. `s` has to start with `"`; everything until the next `"` is
    /// returned in the first component. `\` can be used to escape arbitrary character.
    /// Second return component is the rest of the string with leading whitespace removed.
    fn parse_str<'a>(&mut self, s: &'a str) -> (&'a str, &'a str) {
        let mut chars = s.char_indices();
        match chars.next() {
            Some((_, '"')) => {
                let s = chars.as_str();
                let mut escaped = false;
                for (i, c) in chars {
                    if escaped {
                        // Accept any character as literal after a `\`.
                        escaped = false;
                    } else if c == '"' {
                        return (&s[..(i - 1)], s[i..].trim_start());
                    } else {
                        escaped = c == '\\';
                    }
                }
                self.error(format!("no closing quotes found for {s}"));
                (s, "")
            }
            Some((_, c)) => {
                self.error(format!("expected `\"`, got `{c}`"));
                (s, "")
            }
            None => {
                self.error("expected quoted string, but found end of line");
                (s, "")
            }
        }
    }

    fn parse_pattern(&mut self, pattern: &str, fallthrough_to: &mut Option<usize>) {
        self.parse_pattern_inner(pattern, fallthrough_to, None)
    }

    fn parse_revisioned_pattern(&mut self, pattern: &str, fallthrough_to: &mut Option<usize>) {
        let (revision, pattern) = match pattern.split_once(']') {
            Some(it) => it,
            None => {
                self.error("`//[` without corresponding `]`");
                return;
            }
        };
        if let Some(pattern) = pattern.strip_prefix('~') {
            self.parse_pattern_inner(pattern, fallthrough_to, Some(revision.to_owned()))
        } else {
            self.error("revisioned pattern must have `~` following the `]`");
        }
    }

    // parse something like (?P<offset>\||[\^]+)? *(?P<level>ERROR|HELP|WARN|NOTE): (?P<text>.*)
    fn parse_pattern_inner(
        &mut self,
        pattern: &str,
        fallthrough_to: &mut Option<usize>,
        revision: Option<String>,
    ) {
        let (match_line, pattern) = match pattern.chars().next() {
            Some('|') => (
                match fallthrough_to {
                    Some(fallthrough) => *fallthrough,
                    None => {
                        self.error("`//~|` pattern without preceding line");
                        return;
                    }
                },
                &pattern[1..],
            ),
            Some('^') => {
                let offset = pattern.chars().take_while(|&c| c == '^').count();
                (self.line - offset, &pattern[offset..])
            }
            Some(_) => (self.line, pattern),
            None => {
                self.error("no pattern specified");
                return;
            }
        };

        let pattern = pattern.trim_start();
        let offset = match pattern
            .chars()
            .position(|c| !matches!(c, 'A'..='Z' | 'a'..='z'))
        {
            Some(offset) => offset,
            None => {
                self.error("pattern without level");
                return;
            }
        };

        let level = match pattern[..offset].parse() {
            Ok(level) => level,
            Err(msg) => {
                self.error(msg);
                return;
            }
        };
        let pattern = &pattern[offset..];
        let pattern = match pattern.strip_prefix(':') {
            Some(offset) => offset,
            None => {
                self.error("no `:` after level found");
                return;
            }
        };

        let pattern = pattern.trim();

        self.check(!pattern.is_empty(), "no pattern specified");

        let pattern = self.parse_error_pattern(pattern);

        *fallthrough_to = Some(match_line);

        let definition_line = self.line;
        self.error_matches.push(ErrorMatch {
            pattern,
            revision,
            level,
            definition_line,
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

impl CommentParser {
    fn parse_error_pattern(&mut self, pattern: &str) -> Pattern {
        if let Some(regex) = pattern.strip_prefix('/') {
            match regex.strip_suffix('/') {
                Some(regex) => match self.parse_regex(regex) {
                    Some(regex) => Pattern::Regex(regex),
                    None => Pattern::SubString(pattern.to_string()),
                },
                None => {
                    self.error(
                        "expected regex pattern due to leading `/`, but found no closing `/`",
                    );
                    Pattern::SubString(pattern.to_string())
                }
            }
        } else {
            Pattern::SubString(pattern.to_string())
        }
    }
}
