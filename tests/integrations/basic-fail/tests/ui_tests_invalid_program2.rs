use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let config = Config {
        program: CommandBuilder::cmd("invalid_foobarlaksdfalsdfj"),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error,
        bless_command: Some("DO NOT BLESS. These are meant to fail".to_string()),
        host: Some("foo".into()),
        ..Config::rustc("tests/actual_tests")
    };

    run_tests_generic(
        vec![config],
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
