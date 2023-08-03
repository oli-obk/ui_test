use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    for mode in [
        Mode::Fail {
            require_patterns: true,
            rustfix: true,
        },
        Mode::Yolo { rustfix: true },
    ] {
        let path = "../../../target";

        let root_dir = match mode {
            Mode::Yolo { .. } => "tests/actual_tests_bless_yolo",
            Mode::Fail { .. } => "tests/actual_tests_bless",
            _ => unreachable!(),
        };

        let mut config = Config {
            dependencies_crate_manifest_path: Some("Cargo.toml".into()),
            mode,
            ..Config::rustc(root_dir)
        };
        if std::env::var_os("BLESS").is_some() {
            config.output_conflict_handling = OutputConflictHandling::Bless
        }

        // hide binaries generated for successfully passing tests
        let tmp_dir = tempfile::tempdir_in(path)?;
        let tmp_dir = tmp_dir.path();
        config.out_dir = tmp_dir.into();
        config.path_stderr_filter(tmp_dir, "$TMP");

        config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
        config.path_stderr_filter(&std::path::Path::new(path), "$DIR");
        let result = run_tests_generic(
            vec![config],
            std::num::NonZeroUsize::new(1).unwrap(),
            Args::test(),
            default_file_filter,
            default_per_file_config,
            // Avoid github actions, as these would end up showing up in `Cargo.stderr`
            status_emitter::Text::verbose(),
        );
        match (&result, mode) {
            (Ok(_), Mode::Yolo { .. }) => {}
            (Err(_), Mode::Fail { .. }) => {}
            _ => panic!("invalid mode/result combo: {mode}: {result:?}"),
        }
    }
    Ok(())
}
