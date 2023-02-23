//@run-rustfix
#![deny(warnings)]

fn main() {
    let x: String = 42;
    //~^ ERROR: mismatched types
}
