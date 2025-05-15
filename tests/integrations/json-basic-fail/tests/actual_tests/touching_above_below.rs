fn main() {
    use_unit(1_u32);
    //~^ ERROR: mismatched types
    //~v ERROR: mismatched types
    use_unit(1_u32);
}

fn use_unit(_: ()) {}
