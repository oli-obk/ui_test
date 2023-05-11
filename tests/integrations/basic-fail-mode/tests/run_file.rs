use std::path::Path;
use ui_test::color_eyre::{eyre::ensure, Result};
use ui_test::*;

#[test]
fn run_file() -> Result<()> {
    let mut config = Config::default();

    let tmp_dir = tempfile::tempdir()?;
    config.out_dir = Some(tmp_dir.path().into());

    let mut result = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file.rs"),
    )?;
    ensure!(result.output()?.status.success(), "");
    Ok(())
}

#[test]
fn fail_run_file() {
    let mut config = Config::default();
    config.program = CommandBuilder::cmd("invalid_alsdkfjalsdfjalskdfj");

    let _ = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file.rs"),
    )
    .unwrap()
    .output()
    .unwrap_err();
}

#[test]
fn run_file_no_deps() -> Result<()> {
    let path = "../../../target";

    let mut config = Config::default();

    let tmp_dir = tempfile::tempdir()?;
    config.out_dir = Some(tmp_dir.path().into());

    // Don't build a binary, we only provide .rmeta dependencies for now
    config.program.args.push("--emit=metadata".into());
    config.dependencies_crate_manifest_path = Some("Cargo.toml".into());
    config
        .dependency_builder
        .envs
        .push(("CARGO_TARGET_DIR".into(), Some(path.into())));

    let mut result = ui_test::run_file(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file_with_deps.rs"),
    )?;
    ensure!(result.output()?.status.success(), "");
    Ok(())
}
