use basic_bin::add;
//~^ ERROR: unresolved import `basic_bin`

fn main() {
    add("42", 3);
}
