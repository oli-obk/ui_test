use std::path::{Path, PathBuf};

use crate::rustc_stderr::Level;
use crate::rustc_stderr::Message;

use super::*;

fn config() -> Config {
    Config {
        root_dir: PathBuf::from("$RUSTROOT"),
        program: CommandBuilder::cmd("cake"),
        ..Config::rustc(PathBuf::new())
    }
}

#[test]
fn issue_2156() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address $HEX is unallocated)
}
    ";
    let comments = Comments::parse(s).unwrap();
    let mut errors = vec![];
    let config = config();
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message:"Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            }
        ]
    ];
    check_annotations(
        messages,
        vec![],
        Path::new("moobar"),
        &mut errors,
        &config,
        "",
        &comments,
    )
    .unwrap();
    match &errors[..] {
        [Error::PatternNotFound { pattern, .. }, Error::ErrorsWithoutPattern { path, .. }]
            if path.as_ref().is_some_and(|p| p.line().get() == 5) && pattern.line().get() == 5 => {}
        _ => panic!("{:#?}", errors),
    }
}

#[test]
fn find_pattern() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address 0x10 is unallocated)
}
    ";
    let comments = Comments::parse(s).unwrap();
    let config = config();
    {
        let messages = vec![vec![], vec![], vec![], vec![], vec![], vec![
                Message {
                    message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                    level: Level::Error,
                    line_col: None,
                }
            ]
        ];
        let mut errors = vec![];
        check_annotations(
            messages,
            vec![],
            Path::new("moobar"),
            &mut errors,
            &config,
            "",
            &comments,
        )
        .unwrap();
        match &errors[..] {
            [] => {}
            _ => panic!("{:#?}", errors),
        }
    }

    // only difference to above is a wrong line number
    {
        let messages = vec![vec![], vec![], vec![], vec![], vec![
                Message {
                    message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                    level: Level::Error,
                    line_col: None,
                }
            ]
        ];
        let mut errors = vec![];
        check_annotations(
            messages,
            vec![],
            Path::new("moobar"),
            &mut errors,
            &config,
            "",
            &comments,
        )
        .unwrap();
        match &errors[..] {
            [Error::PatternNotFound { pattern, .. }, Error::ErrorsWithoutPattern { path, .. }]
                if path.as_ref().is_some_and(|p| p.line().get() == 4)
                    && pattern.line().get() == 5 => {}
            _ => panic!("not the expected error: {:#?}", errors),
        }
    }

    // only difference to first is a wrong level
    {
        let messages = vec![
            vec![], vec![], vec![], vec![], vec![],
            vec![
                Message {
                    message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                    level: Level::Note,
                    line_col: None,
                }
            ]
        ];
        let mut errors = vec![];
        check_annotations(
            messages,
            vec![],
            Path::new("moobar"),
            &mut errors,
            &config,
            "",
            &comments,
        )
        .unwrap();
        match &errors[..] {
            // Note no `ErrorsWithoutPattern`, because there are no `//~NOTE` in the test file, so we ignore them
            [Error::PatternNotFound { pattern, .. }] if pattern.line().get() == 5 => {}
            _ => panic!("not the expected error: {:#?}", errors),
        }
    }
}

#[test]
fn duplicate_pattern() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address 0x10 is unallocated)
    //~^ ERROR: encountered a dangling reference (address 0x10 is unallocated)
}
    ";
    let comments = Comments::parse(s).unwrap();
    let config = config();
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            }
        ]
    ];
    let mut errors = vec![];
    check_annotations(
        messages,
        vec![],
        Path::new("moobar"),
        &mut errors,
        &config,
        "",
        &comments,
    )
    .unwrap();
    match &errors[..] {
        [Error::PatternNotFound { pattern, .. }] if pattern.line().get() == 6 => {}
        _ => panic!("{:#?}", errors),
    }
}

#[test]
fn missing_pattern() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address 0x10 is unallocated)
}
    ";
    let comments = Comments::parse(s).unwrap();
    let config = config();
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            },
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            }
        ]
    ];
    let mut errors = vec![];
    check_annotations(
        messages,
        vec![],
        Path::new("moobar"),
        &mut errors,
        &config,
        "",
        &comments,
    )
    .unwrap();
    match &errors[..] {
        [Error::ErrorsWithoutPattern { path, .. }]
            if path.as_ref().is_some_and(|p| p.line().get() == 5) => {}
        _ => panic!("{:#?}", errors),
    }
}

#[test]
fn missing_warn_pattern() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address 0x10 is unallocated)
    //~^ WARN: cake
}
    ";
    let comments = Comments::parse(s).unwrap();
    let config = config();
    let messages= vec![
        vec![],
        vec![],
        vec![],
        vec![],
        vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            },
            Message {
                message: "kaboom".to_string(),
                level: Level::Warn,
                line_col: None,
            },
            Message {
                message: "cake".to_string(),
                level: Level::Warn,
                line_col: None,
            },
        ],
    ];
    let mut errors = vec![];
    check_annotations(
        messages,
        vec![],
        Path::new("moobar"),
        &mut errors,
        &config,
        "",
        &comments,
    )
    .unwrap();
    match &errors[..] {
        [Error::ErrorsWithoutPattern { path, msgs, .. }]
            if path.as_ref().is_some_and(|p| p.line().get() == 5) =>
        {
            match &msgs[..] {
                [Message {
                    message,
                    level: Level::Warn,
                    line_col: _,
                }] if message == "kaboom" => {}
                _ => panic!("{:#?}", msgs),
            }
        }
        _ => panic!("{:#?}", errors),
    }
}

#[test]
fn missing_implicit_warn_pattern() {
    let s = r"
use std::mem;
//@require-annotations-for-level: ERROR
fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address 0x10 is unallocated)
    //~^ WARN: cake
}
    ";
    let comments = Comments::parse(s).unwrap();
    let config = config();
    let messages = vec![
        vec![],
        vec![],
        vec![],
        vec![],
        vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
            },
            Message {
                message: "kaboom".to_string(),
                level: Level::Warn,
                line_col: None,
            },
            Message {
                message: "cake".to_string(),
                level: Level::Warn,
                line_col: None,
            },
        ],
    ];
    let mut errors = vec![];
    check_annotations(
        messages,
        vec![],
        Path::new("moobar"),
        &mut errors,
        &config,
        "",
        &comments,
    )
    .unwrap();
    match &errors[..] {
        [] => {}
        _ => panic!("{:#?}", errors),
    }
}
