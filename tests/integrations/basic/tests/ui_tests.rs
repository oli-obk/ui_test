use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        quiet: false,
        root_dir: "tests/actual_tests".into(),
        dependencies_crate_manifest_path: Some("Cargo.toml".into()),
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error
        },
        num_test_threads: NonZeroUsize::new(1).unwrap(),
        ..Config::default()
    };
    config
        .dependency_builder
        .envs
        .push(("CARGO_TARGET_DIR".into(), path.into()));
    config.args.push("--edition=2021".into());
    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
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

    // hide binaries generated for successfully passing tests
    let tmp_dir = tempfile::tempdir()?;
    config.args.push("--out-dir".into());
    config.args.push(tmp_dir.path().as_os_str().to_owned());

    ui_test::run_tests(config)
}
