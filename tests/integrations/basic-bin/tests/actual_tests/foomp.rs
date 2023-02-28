use basic_bin::add;
//~^ ERROR: extern location for basic_bin is of an unknown type
//~| ERROR: file name should be lib*.rlib or lib*.so
//~| ERROR: can't find crate for `basic_bin`

fn main() {
    add("42", 3);
}
