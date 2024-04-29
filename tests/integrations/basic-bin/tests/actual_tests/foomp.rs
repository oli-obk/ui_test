//@normalize-stderr-test: "(file name should be).*" -> "$1"

use basic_bin::add;
//~^ ERROR: unresolved import

fn main() {
    add("42", 3);
}
