use basic_fail::add;
//@ revisions: foo bar

fn main() {
    add("42", 3);
    //~^ ERROR: mismatched types
}
