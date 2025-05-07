use ui_test::{dependencies::DependencyBuilder, *};

fn main() -> ui_test::color_eyre::Result<()> {
    let path = "../../../target";
    let mut config = Config {
        output_conflict_handling: if std::env::var_os("BLESS").is_some() {
            ui_test::bless_output_files
        } else {
            ui_test::error_on_output_conflict
        },
        bless_command: Some("cargo test".to_string()),
        ..Config::rustc("tests/actual_tests", env!("CARGO_TARGET_TMPDIR"))
    };
    config
        .comment_defaults
        .base()
        .set_custom("dependencies", DependencyBuilder::default());
    config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
    config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
    config.path_stderr_filter(&std::path::Path::new(path), "$DIR");

    run_tests_generic(
        vec![config],
        default_file_filter,
        default_per_file_config,
        // Avoid github actions, as these would end up showing up in `Cargo.stderr`
        status_emitter::Text::verbose(),
    )
}
