use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let config = Config {
        quiet: false,
        program: CommandBuilder::cmd("invalid_foobarlaksdfalsdfj"),
        root_dir: "tests/actual_tests".into(),
        host: Some("foo".into()),
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error(
            "DO NOT BLESS. These are meant to fail".into(),
        ),
        // Make sure our tests are ordered for reliable output.
        num_test_threads: NonZeroUsize::new(1).unwrap(),
        ..Config::default()
    };

    ui_test::run_tests_generic(
        config,
        |path| path.extension().map(|ext| ext == "rs").unwrap_or(false),
        |_, _| None,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text,
    )
}
