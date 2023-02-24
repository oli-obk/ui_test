use std::path::Path;
use ui_test::color_eyre::{eyre::ensure, Result};
use ui_test::*;

#[test]
fn run_file() -> Result<()> {
    let mut config = Config::default();
    // Don't require `extern crate` for imports.
    config.args.push("--edition=2021".into());

    let tmp_dir = tempfile::tempdir()?;
    config.out_dir = Some(tmp_dir.path().into());

    let result = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file.rs"),
    )?;
    ensure!(result.status.success(), "");
    Ok(())
}

#[test]
fn fail_run_file() {
    let mut config = Config::default();
    config.program = "invalid_alsdkfjalsdfjalskdfj".into();

    let _ = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file.rs"),
    )
    .unwrap_err();
}

#[test]
fn run_file_no_deps() -> Result<()> {
    let path = "../../../target";

    let mut config = Config::default();
    // Don't require `extern crate` for imports.
    config.args.push("--edition=2021".into());

    let tmp_dir = tempfile::tempdir()?;
    config.out_dir = Some(tmp_dir.path().into());

    // Don't build a binary, we only provide .rmeta dependencies for now
    config.args.push("--emit=metadata".into());
    config.dependencies_crate_manifest_path = Some("Cargo.toml".into());
    config
        .dependency_builder
        .envs
        .push(("CARGO_TARGET_DIR".into(), path.into()));

    let result = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file_with_deps.rs"),
    )?;
    ensure!(result.status.success(), "");
    Ok(())
}
