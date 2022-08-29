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
        args: vec!["--edition=2021".into()],
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error
        },
        ..Config::default()
    };
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
