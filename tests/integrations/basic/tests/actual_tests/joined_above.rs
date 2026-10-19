// there are overlapping suggestions here that rustfix struggles with
//@no-rustfix
#![deny(unused_mut, unused_variables)]

fn main() {
    //~| unused_mut
    //~v unused_variables
    let mut x = 0u32;
}
