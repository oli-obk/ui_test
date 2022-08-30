use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        quiet: false,
        root_dir: "tests/actual_tests".into(),
        dependencies_crate_manifest_path: Some("tests/dummy_crate/Cargo.toml".into()),
        dependency_builder: Some(DependencyBuilder {
            program: std::path::PathBuf::from("cargo"),
            args: vec![],
            envs: vec![("CARGO_TARGET_DIR".into(), path.into())],
        }),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error,
        ..Config::default()
    };
    config.args.push("--edition=2021".into());
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.stderr_filter(
        &std::path::Path::new(path)
            .canonicalize()
            .unwrap()
            .parent()
            .unwrap()
            .display()
            .to_string(),
        "$$DIR",
    );
    ui_test::run_tests(config)
}
