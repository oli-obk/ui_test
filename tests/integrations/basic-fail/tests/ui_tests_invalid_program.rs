use std::num::NonZeroUsize;
use ui_test::*;

fn main() -> ui_test::color_eyre::Result<()> {
    let config = Config {
        quiet: false,
        program: "invalid_foobarlaksdfalsdfj".into(),
        root_dir: "tests/actual_tests".into(),
        args: vec![],
        // Never bless integrations-fail tests, we want to see stderr mismatches
        output_conflict_handling: OutputConflictHandling::Error,
        // Make sure our tests are ordered for reliable output.
        num_test_threads: NonZeroUsize::new(1).unwrap(),
        ..Config::default()
    };

    ui_test::run_tests(config)
}
