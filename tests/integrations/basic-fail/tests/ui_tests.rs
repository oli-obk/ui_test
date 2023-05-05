use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        quiet: false,
        root_dir: "tests/actual_tests".into(),
        dependencies_crate_manifest_path: Some("Cargo.toml".into()),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error,
        // Make sure our tests are ordered for reliable output.
        num_test_threads: NonZeroUsize::new(1).unwrap(),
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
    
    ui_test::run_tests_generic(
        config,
        |path| path.extension().map(|ext| ext == "rs").unwrap_or(false),
        |_, _| None,
        // Avoid github actions, as these would end up showing up in `CArgo.stderr`
        status_emitter::Text,
    )
}
