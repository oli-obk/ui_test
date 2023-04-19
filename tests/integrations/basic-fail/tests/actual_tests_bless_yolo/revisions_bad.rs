#[cfg(foo)]
use basic_fail::add;
//@ revisions: foo bar
//@[bar] error-pattern: `main` function not found in crate `revisions_bad`

#[cfg(foo)]
fn main() {
    add("42", 3);
    //~[foo]^ ERROR: mismatched types
}
