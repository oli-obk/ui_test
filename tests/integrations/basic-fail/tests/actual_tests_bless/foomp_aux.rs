//@aux-build:foomp.rs

use foomp::add;

fn main() {
    add("42", 3);
    //~^ ERROR: mismatched types
}
