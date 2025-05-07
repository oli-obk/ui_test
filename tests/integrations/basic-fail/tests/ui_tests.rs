use ui_test::{dependencies::DependencyBuilder, *};

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: ui_test::error_on_output_conflict,
        bless_command: Some("DO NOT BLESS. These are meant to fail".to_string()),
        ..Config::rustc("tests/actual_tests", env!("CARGO_TARGET_TMPDIR"))
    };

    config
        .comment_defaults
        .base()
        .set_custom("dependencies", DependencyBuilder::default());

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
        vec![config],
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
