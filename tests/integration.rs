use std::path::Path;

use ui_test::color_eyre::Result;
use ui_test::*;

fn main() -> Result<()> {
    let path = Path::new(file!()).parent().unwrap();
    let root_dir = path.join("integrations");
    let mut config = Config {
        ..Config::cargo(root_dir.clone())
    };
    let args = Args::test()?;
    config.with_args(&args, true);

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
    config.stdout_filter(r#""--out-dir"(,)? "[^"]+""#, r#""--out-dir"$1 "$$TMP"#);
    config.filter("\\.exe", b"");
    config.filter(
        "( *process didn't exit successfully: `.*)-[0-9a-f]+`",
        "$1-HASH`",
    );
    // Windows io::Error uses "exit code".
    config.filter("exit code", "exit status");
    config.filter(
        "The system cannot find the file specified.",
        "No such file or directory",
    );
    config.filter("RUSTC_BOOTSTRAP=\"1\" ", "");
    // The order of the `/deps` directory flag is flaky
    config.stdout_filter("/deps", "");
    config.path_filter(std::path::Path::new(path), "$DIR");
    config.stdout_filter("[0-9a-f]+\\.rmeta", "$$HASH.rmeta");
    // Windows backslashes are sometimes escaped.
    // Insert the replacement filter at the start to make sure the filter for single backslashes
    // runs afterwards.
    config
        .stdout_filters
        .insert(0, (Match::Exact(b"\\\\".to_vec()), b"\\"));
    config.stdout_filter(r#"(panic.*)\.rs:[0-9]+:[0-9]+"#, "$1.rs");
    // We don't want to normalize lines starting with `+`, those are diffs of the inner ui_test
    // and normalizing these here doesn't make the "actual output differed from expected" go
    // away, it just makes it impossible to diagnose.
    config.filter(" +[0-9]+: .*\n", "");
    config.filter("                +at \\.?/.*\n", "");
    config.filter(" running on .*", "");
    config.stdout_filter("/target/[^/]+/[^/]+/debug", "/target/$$TMP/$$TRIPLE/debug");
    config.stdout_filter("/target/.tmp[^/ \"]+", "/target/$$TMP");
    // Normalize proc macro filenames on windows to their linux repr
    config.stdout_filter("/([^/\\.]+)\\.dll", "/lib$1.so");
    // Normalize proc macro filenames on mac to their linux repr
    config.stdout_filter("/([^/\\.]+)\\.dylib", "/$1.so");
    config.stdout_filter("(command: )\"[^<rp][^\"]+", "$1\"$$CMD");
    config.filter("(src/.*?\\.rs):[0-9]+:[0-9]+", "$1:LL:CC");
    config.filter("program not found", "No such file or directory");
    config.filter(" \\(os error [0-9]+\\)", "");
    config.filter("note: rustc 1\\..*", "");

    let text = ui_test::status_emitter::Text::from(args.format);

    run_tests_generic(
        vec![
            Config {
                mode: Mode::Pass,
                ..config.clone()
            },
            Config {
                mode: Mode::Panic,
                ..config
            },
        ],
        |path, config| {
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
                && match config.mode {
                    Mode::Pass => !fail,
                    // This is weird, but `cargo test` returns 101 instead of 1 when
                    // multiple [[test]]s exist. If there's only one test, it returns
                    // 1 on failure.
                    Mode::Panic => fail,
                    Mode::Run { .. } | Mode::Yolo { .. } | Mode::Fail { .. } => unreachable!(),
                }
                && default_any_file_filter(path, config)
        },
        |_, _, _| {},
        (
            text,
            ui_test::status_emitter::Gha::<true> {
                name: "integration tests".into(),
            },
        ),
    )
}
