fn main() {
    //~vvvvvv ERROR: mismatched types
    use_unit(1_u32);
}

fn use_unit(_: ()) {}
