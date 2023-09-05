//@compile-flags: -Ztreat-err-as-bug
//@rustc-env: RUSTC_BOOTSTRAP=1
//@normalize-stderr-test: " +[0-9]+: .*\n" -> ""
//@normalize-stderr-test: "                +at /.*\n" -> ""
//@normalize-stderr-test: " running on .*" -> ""
use basic_fail::add;

fn main() {
    add("42", 3);
    //~^ ERROR: mismatched types
}
