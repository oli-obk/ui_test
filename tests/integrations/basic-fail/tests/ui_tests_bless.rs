use ui_test::{
    custom_flags::rustfix::RustfixMode, dependencies::DependencyBuilder, spanned::Spanned, *,
};

fn main() -> ui_test::color_eyre::Result<()> {
    for rustfix in [RustfixMode::MachineApplicable, RustfixMode::Everything] {
        let path = "../../../target";

        let root_dir = match rustfix {
            RustfixMode::Everything => "tests/actual_tests_bless_yolo",
            RustfixMode::MachineApplicable { .. } => "tests/actual_tests_bless",
            _ => unreachable!(),
        };

        let mut config = Config {
            output_conflict_handling: if std::env::var_os("BLESS").is_some() {
                OutputConflictHandling::Bless
            } else {
                OutputConflictHandling::Error
            },
            bless_command: Some("cargo test".to_string()),
            ..Config::rustc(root_dir)
        };
        config.comment_defaults.base().exit_status = match rustfix {
            RustfixMode::Everything => None.into(),
            RustfixMode::MachineApplicable { .. } => Spanned::dummy(1).into(),
            _ => unreachable!(),
        };
        config
            .comment_defaults
            .base()
            .set_custom("rustfix", rustfix);

        config
            .comment_defaults
            .base()
            .set_custom("dependencies", DependencyBuilder::default());

        // hide binaries generated for successfully passing tests
        let tmp_dir = tempfile::tempdir_in(path)?;
        let tmp_dir = tmp_dir.path();
        config.out_dir = tmp_dir.into();
        config.path_stderr_filter(tmp_dir, "$TMP");

        config.stderr_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stdout_filter("in ([0-9]m )?[0-9\\.]+s", "");
        config.stderr_filter(r"[^ ]*/\.?cargo/registry/.*/", "$$CARGO_REGISTRY");
        config.path_stderr_filter(&std::path::Path::new(path), "$DIR");

        // This is part of a test: we want to make sure the base filters are run first
        config.stderr_filter("normalization_override", "NORMALIZATION_OVERRIDE");

        let _ = run_tests_generic(
            vec![config],
            default_file_filter,
            default_per_file_config,
            // Avoid github actions, as these would end up showing up in `Cargo.stderr`
            status_emitter::Text::verbose(),
        );
    }
    Ok(())
}
