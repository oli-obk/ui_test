//@compile-flags: -Ztreat-err-as-bug
//@rustc-env: RUSTC_BOOTSTRAP=1
//@rustc-env: RUSTC_ICE=0
//@normalize-stderr-test: "(?s)(thread 'rustc' panicked).*end of query stack" -> "$1"
use basic_fail::add;

fn main() {
    add("42", 3);
    //~^ ERROR: mismatched types
}
