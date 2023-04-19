use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    for mode in [
        Mode::Fail {
            require_patterns: true,
        },
        Mode::Yolo,
    ] {
        let path = "../../../target";
        let mut config = Config {
            quiet: false,
            root_dir: "tests/actual_tests_bless".into(),
            dependencies_crate_manifest_path: Some("Cargo.toml".into()),
            output_conflict_handling: if std::env::var_os("BLESS").is_some() {
                OutputConflictHandling::Bless
            } else {
                OutputConflictHandling::Error
            },
            // Make sure our tests are ordered for reliable output.
            num_test_threads: NonZeroUsize::new(1).unwrap(),
            mode,
            ..Config::default()
        };
        config
            .dependency_builder
            .envs
            .push(("CARGO_TARGET_DIR".into(), Some(path.into())));

        // hide binaries generated for successfully passing tests
        let tmp_dir = tempfile::tempdir()?;
        config.out_dir = Some(tmp_dir.path().into());

        config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
        config.path_stderr_filter(&std::path::Path::new(path), "$DIR");
        let result = ui_test::run_tests(config);
        match (&result, mode) {
            (Ok(_), Mode::Yolo) => {}
            (Err(_), Mode::Fail { .. }) => {}
            _ => panic!("invalid mode/result combo: {mode}: {result:?}"),
        }
    }
    Ok(())
}
