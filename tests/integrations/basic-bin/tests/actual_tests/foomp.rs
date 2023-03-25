use basic_bin::add;
//~^ ERROR: extern location for basic_bin is of an unknown type
//~| ERROR: file name should be
//~| ERROR: can't find crate for `basic_bin`

//@normalize-stderr-test: "(file name should be).*" -> "$1"

fn main() {
    add("42", 3);
}
