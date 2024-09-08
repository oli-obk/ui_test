//@normalize-stderr-test: "NORMALIZATION_OVERRIDE" -> "SUCCESS"

// Test that the above normalization will be run after the default normalization set in ui_tests_bless.rs

fn main() {
    asdf //~ ERROR: cannot find
}
