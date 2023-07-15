//@revisions: a b
#![deny(warnings)]

fn main() {
    let x = match 0 {
        0 => String::new()
        //~^ ERROR: expected `,`
        _ => return,
    };

    x;
    x;
}
