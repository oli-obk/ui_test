use ui_test::{run_tests, Config};

fn main() -> ui_test::color_eyre::Result<()> {
    // Compile all `.rs` files in the given directory (relative to your
    // Cargo.toml) and compare their output against the corresponding
    // `.stderr` files.
    run_tests(Config::rustc("examples_tests/rustc_basic"))
}
