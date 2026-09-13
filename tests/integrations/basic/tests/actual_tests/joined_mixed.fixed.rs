#![deny(unused_mut, unused_variables)]

fn main() {
    let _x = 0u32;
    //~^ unused_variables
    //~| unused_mut

    //~| unused_mut
    //~v unused_variables
    let _y = 0u32;
}
