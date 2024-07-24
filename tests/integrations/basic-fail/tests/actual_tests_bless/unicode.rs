#![deny(confusable_idents)]

// Test that a missing annotation causing a
// ui_test diagnostic will not ICE due to byte vs char offsets.

fn main() {
    let Ě电脑 = 1;
}
