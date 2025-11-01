use basic_fail::add;

fn main() {
    add("42", 3);
    //~^ ERROR: miesmätsched types
    add("42", 3);
    //~^ ERROR: miesmätsched types
}
