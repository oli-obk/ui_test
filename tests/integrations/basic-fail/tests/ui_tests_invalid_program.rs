use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let config = Config {
        program: CommandBuilder::cmd("invalid_foobarlaksdfalsdfj"),
        ..Config::rustc("tests/actual_tests")
    };

    run_tests_generic(
        vec![config],
        // Never bless integrations-fail tests, we want to see stderr mismatches
        Args::test(false)?,
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
