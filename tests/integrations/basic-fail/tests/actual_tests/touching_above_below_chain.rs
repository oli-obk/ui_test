fn main() {
    use_unit(1_u32);
    //~^ ERROR: mismatched types
    //~| ERROR: variable does not need to be mutable
    //~v ERROR: unused variable
    let mut x = 0u32;
}

fn use_unit(_: ()) {}
