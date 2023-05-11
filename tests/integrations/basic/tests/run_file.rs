use std::path::Path;
use ui_test::color_eyre::{eyre::ensure, Result};
use ui_test::*;

#[test]
fn run_file() -> Result<()> {
    let mut config = Config::default();

    let tmp_dir = tempfile::tempdir()?;
    config.out_dir = Some(tmp_dir.path().into());

    let mut result = ui_test::test_command(
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
fn run_file_with_deps() -> Result<()> {
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

    let mut result = ui_test::test_command(
        config,
        &Path::new(file!())
            .parent()
            .unwrap()
            .join("run_file/run_file_with_deps.rs"),
    )?;
    ensure!(result.output()?.status.success(), "");
    Ok(())
}

#[test]
fn non_utf8() -> Result<()> {
    let path = Path::new(file!())
        .parent()
        .unwrap()
        .join("run_file/non_utf8");
    if cfg!(windows) {
        return Ok(());
    }
    let mut config = Config::default();
    config.program = CommandBuilder::cmd("cat");
    config.edition = None;

    let mut result = ui_test::test_command(config, &path)?;
    ensure!(result.output()?.status.success(), "");
    Ok(())
}
