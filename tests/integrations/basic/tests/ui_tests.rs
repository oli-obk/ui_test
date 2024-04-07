use ui_test::{dependencies::DependencyBuilder, spanned::Spanned, *};

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error
        },
        bless_command: Some("cargo test".to_string()),
        ..Config::rustc("tests/actual_tests")
    };
    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");
    config.comment_defaults.base().custom.insert(
        "dependencies",
        Spanned::dummy(vec![Box::new(DependencyBuilder::default())]),
    );

    if let Ok(target) = std::env::var("UITEST_TEST_TARGET") {
        config.target = Some(target);
        config.output_conflict_handling = OutputConflictHandling::Ignore;
    }

    // hide binaries generated for successfully passing tests
    let tmp_dir = tempfile::tempdir_in(path)?;
    let tmp_dir = tmp_dir.path();
    config.out_dir = tmp_dir.into();
    config.path_stderr_filter(tmp_dir, "$TMP");

    run_tests_generic(
        vec![config],
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
