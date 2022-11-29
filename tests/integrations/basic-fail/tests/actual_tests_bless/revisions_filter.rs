use basic_fail::add;
//@ignore-on-host
//@ revisions: foo bar

#[cfg(foo)]
fn main() {
    add("42", 3);
}

#[cfg(bar)]
fn main() {
    add("42", 3);
    //~[bar]^ ERROR: mismatched types
}
