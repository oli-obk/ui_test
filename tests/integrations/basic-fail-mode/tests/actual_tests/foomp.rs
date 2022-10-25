use basic_fail_mode::add;

fn main() {
    add("42", 3);
    //~^ ERROR: mismatched types
}
