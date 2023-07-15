use std::path::Path;

use clap::Parser;
use colored::Colorize;
use ui_test::color_eyre::Result;
use ui_test::*;

fn main() -> Result<()> {
    run("integrations", Mode::Pass)?;
    run("integrations", Mode::Panic)?;

    eprintln!("integration tests done");

    Ok(())
}

fn run(name: &str, mode: Mode) -> Result<()> {
    eprintln!("\n{} `{name}` tests in mode {mode}", "Running".green());
    let path = Path::new(file!()).parent().unwrap();
    let root_dir = path.join(name);
    let mut config = Config {
        mode,
        ..Config::cargo(root_dir.clone())
    };
    let args = Args::parse();

    if !args.check {
        config.output_conflict_handling = OutputConflictHandling::Bless;
    }

    config.program.args = vec![
        "test".into(),
        "--color".into(),
        "never".into(),
        "--quiet".into(),
        "--jobs".into(),
        "1".into(),
        "--no-fail-fast".into(),
    ];
    config
        .program
        .envs
        .push(("RUST_TEST_THREADS".into(), Some("1".into())));

    config
        .program
        .envs
        .push(("BLESS".into(), (!args.check).then(|| String::new().into())));

    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r#""--out-dir"(,)? "[^"]+""#, r#""--out-dir"$1 "$$TMP"#);
    config.stderr_filter(
        "( *process didn't exit successfully: `[^-]+)-[0-9a-f]+",
        "$1-HASH",
    );
    // Windows io::Error uses "exit code".
    config.stderr_filter("exit code", "exit status");
    // The order of the `/deps` directory flag is flaky
    config.stderr_filter("/deps", "");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");
    config.stderr_filter("[0-9a-f]+\\.rmeta", "$$HASH.rmeta");
    // Windows backslashes are sometimes escaped.
    // Insert the replacement filter at the start to make sure the filter for single backslashes
    // runs afterwards.
    config
        .stderr_filters
        .insert(0, (Match::Exact(b"\\\\".iter().copied().collect()), b"\\"));
    config.stderr_filter("\\.exe", b"");
    config.stderr_filter(r#"(panic.*)\.rs:[0-9]+:[0-9]+"#, "$1.rs");
    config.stderr_filter("   [0-9]: .*", "");
    config.stderr_filter("/target/[^/]+/[^/]+/debug", "/target/$$TMP/$$TRIPLE/debug");
    config.stderr_filter("/target/[^/]+/tests", "/target/$$TMP/tests");
    // Normalize proc macro filenames on windows to their linux repr
    config.stderr_filter("/([^/\\.]+)\\.dll", "/lib$1.so");
    // Normalize proc macro filenames on mac to their linux repr
    config.stderr_filter("/([^/\\.]+)\\.dylib", "/$1.so");
    config.stderr_filter("(command: )\"[^<rp][^\"]+", "$1\"$$CMD");
    config.stderr_filter("(src/.*?\\.rs):[0-9]+:[0-9]+", "$1:LL:CC");
    config.stderr_filter("program not found", "No such file or directory");
    config.stderr_filter(" \\(os error [0-9]+\\)", "");

    run_tests_generic(
        config,
        args,
        |path, args| {
            let fail = path
                .parent()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .ends_with("-fail");
            if cfg!(windows) && path.components().any(|c| c.as_os_str() == "basic-bin") {
                // on windows there's also a .pdb file, so we get additional errors that aren't there on other platforms
                return false;
            }
            path.ends_with("Cargo.toml")
                && path.parent().unwrap().parent().unwrap() == root_dir
                && match mode {
                    Mode::Pass => !fail,
                    // This is weird, but `cargo test` returns 101 instead of 1 when
                    // multiple [[test]]s exist. If there's only one test, it returns
                    // 1 on failure.
                    Mode::Panic => fail,
                    Mode::Fix | Mode::Run { .. } | Mode::Yolo | Mode::Fail { .. } => unreachable!(),
                }
                && default_filter_by_arg(path, args)
        },
        |_, _| None,
        (
            ui_test::status_emitter::Text::verbose(),
            ui_test::status_emitter::Gha::<true> {
                name: format!("{mode:?}"),
            },
        ),
    )
}
