use ui_test::{spanned::Spanned, *};

fn main() -> ui_test::color_eyre::Result<()> {
    let mut config = Config {
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            ui_test::bless_output_files
        } else {
            ui_test::error_on_output_conflict
        },
        bless_command: Some("cargo test".to_string()),
        ..Config::cargo("tests/actual_tests", env!("CARGO_TARGET_TMPDIR"))
    };
    config.comment_defaults.base().exit_status = Spanned::dummy(101).into();
    config.comment_defaults.base().require_annotations = Spanned::dummy(false).into();

    config.program.args = vec!["run".into(), "--quiet".into()];
    config.program.input_file_flag = Some("--".into());

    run_tests_generic(
        vec![config],
        default_file_filter,
        |_config, _content| {},
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
