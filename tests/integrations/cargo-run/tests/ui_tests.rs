use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let mut config = Config {
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            OutputConflictHandling::Bless
        } else {
            OutputConflictHandling::Error("cargo test".to_string())
        },
        mode: Mode::Panic,
        ..Config::cargo("tests/actual_tests")
    };

    config.program.args = vec!["run".into(), "--quiet".into()];
    config.program.input_file_flag = Some("--".into());
    config.program.out_dir_flag = None;

    run_tests_generic(
        vec![config],
        default_file_filter,
        |_config, _path, _content| {},
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
