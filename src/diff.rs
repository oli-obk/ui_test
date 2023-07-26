use colored::*;
use prettydiff::{basic::DiffOp, diff_lines, diff_words};

#[derive(Default)]
struct DiffState;

/// How many lines of context are displayed around the actual diffs
const CONTEXT: usize = 2;

impl<'a> DiffState {
    fn skip(&mut self, skipped_lines: &[&'a str]) {
        // When the amount of skipped lines is exactly `CONTEXT * 2`, we already
        // print all the context and don't actually skip anything.
        match skipped_lines.len().checked_sub(CONTEXT * 2) {
            Some(skipped @ 2..) => {
                // Print an initial `CONTEXT` amount of lines.
                for line in &skipped_lines[..CONTEXT] {
                    eprintln!(" {line}");
                }
                eprintln!("... {skipped} lines skipped ...");
                // Print `... n lines skipped ...` followed by the last `CONTEXT` lines.
                for line in &skipped_lines[skipped + CONTEXT..] {
                    eprintln!(" {line}");
                }
            }
            _ => {
                // Print all the skipped lines if the amount of context desired is less than the amount of lines
                for line in skipped_lines {
                    eprintln!(" {line}");
                }
            }
        }
    }

    fn print_left(&self, l: &str) {
        eprintln!("{}{}", "-".red(), l.red());
    }

    fn print_right(&self, r: &str) {
        eprintln!("{}{}", "+".green(), r.green());
    }

    fn row(&mut self, row: DiffOp<&'a str>) {
        use prettydiff::basic::DiffOp::*;
        match row {
            Remove(l) => {
                for l in l {
                    self.print_left(l);
                }
            }
            Equal(l) => {
                self.skip(l);
            }
            Replace(l, r) => {
                for (l, r) in l.iter().zip(r) {
                    let diff = diff_words(l, r);
                    let diff = diff.diff();
                    let mut seen_l = false;
                    let mut seen_r = false;
                    for char in &diff {
                        let is_whitespace =
                            |s: &[&str]| s.iter().any(|s| s.chars().any(|s| s.is_whitespace()));
                        match char {
                            Insert(l) if !is_whitespace(l) => seen_l = true,
                            Remove(r) if !is_whitespace(r) => seen_r = true,
                            Replace(l, r) if !is_whitespace(l) && !is_whitespace(r) => {
                                seen_l = true;
                                seen_r = true;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if seen_l && seen_r {
                        // The line both adds and removes chars, print both lines, but highlight their differences instead of
                        // drawing the entire line in red/green.
                        eprint!("{}", "-".red());
                        for char in &diff {
                            match *char {
                                Replace(l, _) | Remove(l) => {
                                    for l in l {
                                        eprint!("{}", l.to_string().on_red())
                                    }
                                }
                                Insert(_) => {}
                                Equal(l) => {
                                    for l in l {
                                        eprint!("{l}")
                                    }
                                }
                            }
                        }
                        eprintln!();
                        eprint!("{}", "+".green());
                        for char in diff {
                            match char {
                                Remove(_) => {}
                                Replace(_, r) | Insert(r) => {
                                    for r in r {
                                        eprint!("{}", r.to_string().on_green())
                                    }
                                }
                                Equal(r) => {
                                    for r in r {
                                        eprint!("{r}")
                                    }
                                }
                            }
                        }
                        eprintln!();
                    } else {
                        // The line only adds or only removes chars, print a single line highlighting their differences.
                        eprint!("{}", "~".yellow());
                        for char in diff {
                            match char {
                                Remove(l) => {
                                    for l in l {
                                        eprint!("{}", l.to_string().on_red())
                                    }
                                }
                                Equal(w) => {
                                    for w in w {
                                        eprint!("{w}")
                                    }
                                }
                                Insert(r) => {
                                    for r in r {
                                        eprint!("{}", r.to_string().on_green())
                                    }
                                }
                                Replace(..) => unreachable!(),
                            }
                        }
                        eprintln!();
                    }
                }
            }
            Insert(r) => {
                for r in r {
                    self.print_right(r);
                }
            }
        }
    }

    fn finish(self) {
        eprintln!()
    }
}

pub fn print_diff(expected: &[u8], actual: &[u8]) {
    let expected_str = String::from_utf8_lossy(expected);
    let actual_str = String::from_utf8_lossy(actual);

    if expected_str.as_bytes() != expected || actual_str.as_bytes() != actual {
        eprintln!(
            "{}",
            "Non-UTF8 characters in output, diff may be imprecise.".red()
        );
    }

    let pat = |c: char| c.is_whitespace() && c != ' ' && c != '\n' && c != '\r';
    let expected_str = expected_str.replace(pat, "░");
    let actual_str = actual_str.replace(pat, "░");

    let mut state = DiffState::default();
    for row in diff_lines(&expected_str, &actual_str).diff() {
        state.row(row);
    }
    state.finish();
}
