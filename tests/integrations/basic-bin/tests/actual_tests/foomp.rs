//@normalize-stderr-test: "(file name should be).*" -> "$1"

use basic_bin::add;
//~^ ERROR: can't find crate for `basic_bin`
//~| ERROR: file name should be
//~| ERROR: extern location for basic_bin is of an unknown type

fn main() {
    add("42", 3);
}
