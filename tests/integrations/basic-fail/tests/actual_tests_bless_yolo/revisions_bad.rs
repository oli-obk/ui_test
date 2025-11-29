#[cfg(foo)]
use basic_fail::add;
//@ revisions: foo bar

#[cfg(foo)]
fn main() {
    add("42", 3);
    //~[foo]^ ERROR: mismatched types
}
//~[bar]^ ERROR: `main` function not found in crate `revisions_bad`
