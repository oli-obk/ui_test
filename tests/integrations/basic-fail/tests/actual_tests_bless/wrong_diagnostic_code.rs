#![deny(dead_code, unreachable_code)]

fn foo() -> i32 {
    //~^ should_be_dead_code
    panic!();
    0 //~ unreachable_code
}

fn main() {}
