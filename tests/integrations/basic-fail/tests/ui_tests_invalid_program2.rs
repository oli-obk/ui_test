use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let config = Config {
        program: CommandBuilder::cmd("invalid_foobarlaksdfalsdfj"),
        host: Some("foo".into()),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error(
            "DO NOT BLESS. These are meant to fail".into(),
        ),
        // Make sure our tests are ordered for reliable output.
        num_test_threads: NonZeroUsize::new(1).unwrap(),
        ..Config::rustc("tests/actual_tests")
    };

    run_tests_generic(
        config,
        Args::default(),
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
