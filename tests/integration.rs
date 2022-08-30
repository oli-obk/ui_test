use std::path::Path;

use ui_test::color_eyre::Result;
use ui_test::*;

fn main() -> Result<()> {
    run("integrations")
}

fn run(name: &str) -> Result<()> {
    let path = Path::new(file!()).parent().unwrap();
    let root_dir = path.join(name);
    let mut config = Config {
        root_dir: root_dir.clone(),
        args: vec![
            "test".into(),
            "--target-dir".into(),
            path.parent().unwrap().join("target").into(),
            "--manifest-path".into(),
        ],
        program: "cargo".into(),
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error
        },
        mode: Mode::Pass,
        ..Config::default()
    };

    config.stderr_filter("in [0-9\\.]+s", "");
    config.stderr_filter("(     Running [^(]+).*", "$1");
    config.stderr_filter("   Compiling .*\n", "");

    run_tests_generic(config, |path| {
        path.ends_with("Cargo.toml") && path.parent().unwrap().parent().unwrap() == root_dir
    })
}
