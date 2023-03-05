use std::path::Path;

use colored::Colorize;
use ui_test::color_eyre::Result;
use ui_test::*;

fn main() -> Result<()> {
    run("integrations", Mode::Pass)?;
    run("integrations", Mode::Panic)?;

    Ok(())
}

fn run(name: &str, mode: Mode) -> Result<()> {
    eprintln!("\n{} `{name}` tests in mode {mode}", "Running".green());
    let path = Path::new(file!()).parent().unwrap();
    let root_dir = path.join(name);
    let mut config = Config {
        root_dir: root_dir.clone(),
        args: vec![
            "test".into(),
            "--color".into(),
            "never".into(),
            "--quiet".into(),
            "--jobs".into(),
            "1".into(),
            "--no-fail-fast".into(),
            "--target-dir".into(),
            path.parent().unwrap().join("target").into(),
            "--manifest-path".into(),
        ],
        trailing_args: vec!["--".into(), "--test-threads".into(), "1".into()],
        program: "cargo".into(),
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error
        },
        mode,
        ..Config::default()
    };

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
    config.stderr_filter("failed to parse rustc version info.*", "");

    run_tests_generic(config, |path| {
        let fail = path
            .parent()
            .unwrap()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .ends_with("-fail");
        path.ends_with("Cargo.toml")
            && path.parent().unwrap().parent().unwrap() == root_dir
            && match mode {
                Mode::Pass => !fail,
                // This is weird, but `cargo test` returns 101 instead of 1 when
                // multiple [[test]]s exist. If there's only one test, it returns
                // 1 on failure.
                Mode::Panic => fail,
                Mode::Fail { .. } => unreachable!(),
            }
    })
}
