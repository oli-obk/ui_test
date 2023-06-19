use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        root_dir: "tests/actual_tests".into(),
        dependencies_crate_manifest_path: Some("Cargo.toml".into()),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error(
            "DO NOT BLESS. These are meant to fail".into(),
        ),
        // Make sure our tests are ordered for reliable output.
        num_test_threads: NonZeroUsize::new(1).unwrap(),
        ..Config::default()
    };

    // hide binaries generated for successfully passing tests
    let tmp_dir = tempfile::tempdir_in(path)?;
    let tmp_dir = tmp_dir.path();
    config.out_dir = tmp_dir.into();
    config.path_stderr_filter(tmp_dir, "$TMP");

    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");

    run_tests_generic(
        config,
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text,
    )
}
