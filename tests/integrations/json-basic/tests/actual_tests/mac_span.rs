#![deny(unused_variables)]

macro_rules! m {
    () => {{
        let x = 0; //~ unused_variables
    }};
}

fn main() {
    m!();
}
