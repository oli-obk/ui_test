#![deny(unused_variables)]

macro_rules! m {
    () => {{
        let _x = 0; //~ unused_variables
    }};
}

fn main() {
    m!();
}
