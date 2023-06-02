use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        quiet: false,
        root_dir: "tests/actual_tests".into(),
        dependencies_crate_manifest_path: Some("Cargo.toml".into()),
        ..Config::default()
    };
    if std::env::var_os("BLESS").is_some() {
        config.output_conflict_handling = OutputConflictHandling::Bless
    }
    config
        .dependency_builder
        .envs
        .push(("CARGO_TARGET_DIR".into(), Some(path.into())));
    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.stderr_filter(r"\.exe", "");
    config.stderr_filter("/target/[^/]+/debug", "/target/$$TRIPLE/debug");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");

    run_tests_generic(
        config,
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text,
    )
}
