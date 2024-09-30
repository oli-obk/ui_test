use ui_test::{
    default_file_filter, default_per_file_config, dependencies::DependencyBuilder,
    run_tests_generic, status_emitter::Text, Args, Config,
};

fn main() -> ui_test::Result<()> {
    let mut config = Config {
        output_conflict_handling: ui_test::ignore_output_conflict,
        ..Config::rustc("tests/ui")
    };
    config
        .comment_defaults
        .base()
        .set_custom("dependencies", DependencyBuilder::default());

    let args = Args::test()?;

    config.with_args(&args);

    run_tests_generic(
        [config].into(),
        default_file_filter,
        default_per_file_config,
        Text::verbose(),
    )
}
