use json_basic::add;

fn main() {
    //~v ERROR: mismatched types
    add("42", 3);
}
