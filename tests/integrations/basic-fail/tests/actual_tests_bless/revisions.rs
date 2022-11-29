use basic_fail::add;
//@ revisions: foo bar

#[cfg(foo)]
fn main() {
    add("42", 3); //[foo]~ ERROR: mismatched types
}

#[cfg(bar)]
fn main() {
    add("42", 3);
    //[bar]~^ ERROR: mismatched types
}
