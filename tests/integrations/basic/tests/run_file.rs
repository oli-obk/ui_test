use std::path::{Path, PathBuf};
use ui_test::color_eyre::{eyre::ensure, Result};
use ui_test::dependencies::DependencyBuilder;
use ui_test::spanned::Spanned;
use ui_test::*;

#[test]
fn run_file() -> Result<()> {
    let mut config = Config::rustc(PathBuf::new());

    let tmp_dir = tempfile::tempdir_in(env!("CARGO_TARGET_TMPDIR"))?;
    let tmp_dir = tmp_dir.path();
    config.out_dir = tmp_dir.into();
    config.path_stderr_filter(tmp_dir, "$TMP");

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

    let mut config = Config::rustc(PathBuf::new());

    let tmp_dir = tempfile::tempdir_in(path)?;
    let tmp_dir = tmp_dir.path();
    config.out_dir = tmp_dir.into();
    config.path_stderr_filter(tmp_dir, "$TMP");

    // Don't build a binary, we only provide .rmeta dependencies for now
    config.program.args.push("--emit=metadata".into());
    config.comment_defaults.base().custom.insert(
        "dependencies",
        Spanned::dummy(vec![Box::new(DependencyBuilder::default())]),
    );

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
    let mut config = Config::rustc(PathBuf::new());
    config.program = CommandBuilder::cmd("cat");
    config.comment_defaults.base().custom.clear();
    config.host = Some(String::new());

    let mut result = ui_test::test_command(config, &path)?;
    ensure!(result.output()?.status.success(), "");
    Ok(())
}
