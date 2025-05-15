fn main() {
    //~| ERROR: mismatched types
    use_unit(1_u32);
    //~| ERROR: mismatched types
}

fn use_unit(_: ()) {}
