use std::path::{Path, PathBuf};

use spanned::Spanned;

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

macro_rules! config {
    ($config:ident = $s:expr) => {
        let path = Path::new("moobar");
        let comments = Comments::parse($s, $config.comment_defaults.clone(), path).unwrap();
        #[allow(unused_mut)]
        let mut $config = TestConfig {
            config: $config,
            path,
            comments: &comments,
            revision: "",
        };
    };
}

#[test]
fn issue_2156() {
    let s = r"
use std::mem;

fn main() {
    let _x: &i32 = unsafe { mem::transmute(16usize) }; //~ ERROR: encountered a dangling reference (address $HEX is unallocated)
}
    ";
    let config = config();
    config!(config = s);
    let mut errors = vec![];
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message:"Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
                code: None,
            }
        ]
    ];
    config
        .check_annotations(messages, vec![], &mut errors)
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
    let config = config();
    config!(config = s);
    {
        let messages = vec![vec![], vec![], vec![], vec![], vec![], vec![
                Message {
                    message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                    level: Level::Error,
                    line_col: None,
                    code: None,
                }
            ]
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
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
                    code: None,
                }
            ]
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
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
                    code: None,
                }
            ]
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
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
    let config = config();
    config!(config = s);
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
                code: None,
            }
        ]
    ];
    let mut errors = vec![];
    config
        .check_annotations(messages, vec![], &mut errors)
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
    let config = config();
    config!(config = s);
    let messages = vec![
        vec![], vec![], vec![], vec![], vec![],
        vec![
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
                code: None,
            },
            Message {
                message: "Undefined Behavior: type validation failed: encountered a dangling reference (address 0x10 is unallocated)".to_string(),
                level: Level::Error,
                line_col: None,
                code: None,
            }
        ]
    ];
    let mut errors = vec![];
    config
        .check_annotations(messages, vec![], &mut errors)
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
    let config = config();
    config!(config = s);
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
                code: None,
            },
            Message {
                message: "kaboom".to_string(),
                level: Level::Warn,
                line_col: None,
                code: None,
            },
            Message {
                message: "cake".to_string(),
                level: Level::Warn,
                line_col: None,
                code: None,
            },
        ],
    ];
    let mut errors = vec![];
    config
        .check_annotations(messages, vec![], &mut errors)
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
                    code: None,
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
    let config = config();
    config!(config = s);
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
                code: None,
            },
            Message {
                message: "kaboom".to_string(),
                level: Level::Warn,
                line_col: None,
                code: None,
            },
            Message {
                message: "cake".to_string(),
                level: Level::Warn,
                line_col: None,
                code: None,
            },
        ],
    ];
    let mut errors = vec![];
    config
        .check_annotations(messages, vec![], &mut errors)
        .unwrap();
    match &errors[..] {
        [] => {}
        _ => panic!("{:#?}", errors),
    }
}

#[test]
fn find_code() {
    let s = r"
fn main() {
    let _x: i32 = 0u32; //~ E0308
}
    ";
    let config = config();
    config!(config = s);
    {
        let messages = vec![
            vec![],
            vec![],
            vec![],
            vec![Message {
                message: "mismatched types".to_string(),
                level: Level::Error,
                line_col: None,
                code: Some("E0308".into()),
            }],
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
            .unwrap();
        match &errors[..] {
            [] => {}
            _ => panic!("{:#?}", errors),
        }
    }

    // different error code
    {
        let messages = vec![
            vec![],
            vec![],
            vec![],
            vec![Message {
                message: "mismatched types".to_string(),
                level: Level::Error,
                line_col: None,
                code: Some("SomeError".into()),
            }],
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
            .unwrap();
        match &errors[..] {
            [Error::CodeNotFound { code, .. }, Error::ErrorsWithoutPattern { msgs, .. }]
                if **code == "E0308" && code.line().get() == 3 && msgs.len() == 1 => {}
            _ => panic!("{:#?}", errors),
        }
    }

    // warning instead of error
    {
        let messages = vec![
            vec![],
            vec![],
            vec![],
            vec![Message {
                message: "mismatched types".to_string(),
                level: Level::Warn,
                line_col: None,
                code: Some("E0308".into()),
            }],
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
            .unwrap();
        match &errors[..] {
            [Error::CodeNotFound { code, .. }] if **code == "E0308" && code.line().get() == 3 => {}
            _ => panic!("{:#?}", errors),
        }
    }
}

#[test]
fn find_code_with_prefix() {
    let s = r"
fn main() {
    let _x: i32 = 0u32; //~ E0308
}
    ";
    let mut config = config();
    config.comment_defaults.base().diagnostic_code_prefix =
        Spanned::dummy("prefix::".to_string()).into();
    config!(config = s);
    {
        let messages = vec![
            vec![],
            vec![],
            vec![],
            vec![Message {
                message: "mismatched types".to_string(),
                level: Level::Error,
                line_col: None,
                code: Some("prefix::E0308".into()),
            }],
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
            .unwrap();
        match &errors[..] {
            [] => {}
            _ => panic!("{:#?}", errors),
        }
    }

    // missing prefix
    {
        let messages = vec![
            vec![],
            vec![],
            vec![],
            vec![Message {
                message: "mismatched types".to_string(),
                level: Level::Error,
                line_col: None,
                code: Some("E0308".into()),
            }],
        ];
        let mut errors = vec![];
        config
            .check_annotations(messages, vec![], &mut errors)
            .unwrap();
        match &errors[..] {
            [Error::CodeNotFound { code, .. }, Error::ErrorsWithoutPattern { msgs, .. }]
                if **code == "prefix::E0308" && code.line().get() == 3 && msgs.len() == 1 => {}
            _ => panic!("{:#?}", errors),
        }
    }
}
