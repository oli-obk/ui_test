use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        dependencies_crate_manifest_path: Some("Cargo.toml".into()),
        ..Config::rustc("tests/actual_tests")
    };
    if std::env::var_os("BLESS").is_some() {
        config.output_conflict_handling = OutputConflictHandling::Bless
    }
    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.stderr_filter(r"\.exe", "");
    config.stderr_filter("/target/[^/]+/[^/]+/debug", "/target/$$TMP/$$TRIPLE/debug");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");

    // hide binaries generated for successfully passing tests
    let tmp_dir = tempfile::tempdir_in(path)?;
    let tmp_dir = tmp_dir.path();
    config.out_dir = tmp_dir.into();
    config.path_stderr_filter(tmp_dir, "$$TMP");

    run_tests_generic(
        config,
        Args::default(),
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
