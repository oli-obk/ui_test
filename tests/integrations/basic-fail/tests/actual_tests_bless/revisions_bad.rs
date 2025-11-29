#[cfg(foo)]
use basic_fail::add;
//@ revisions: foo bar
//@[bar] error-in-other-file: `main` function not found in crate `revisions_bad`

#[cfg(foo)]
fn main() {
    add("42", 3);
    //~[foo]^ ERROR: mismatched types
}
