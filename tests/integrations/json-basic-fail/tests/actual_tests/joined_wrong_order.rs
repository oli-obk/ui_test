#![deny(unused_mut, unused_variables)]

fn main() {
    //~vv unused_variables
    //~| unused_mut
    let mut x = 0u32;
}
