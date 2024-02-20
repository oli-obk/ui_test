#![deny(unused_mut, unused_variables)]

fn main() {
    let mut x = 0u32;
    //~^ unused_variables
    //~| unused_mut
}
