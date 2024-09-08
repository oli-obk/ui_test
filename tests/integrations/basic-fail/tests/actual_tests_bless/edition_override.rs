//@edition:2018
//@check-pass

fn main() {
    match 42 {
        0...10 => {} //~ WARN: deprecated
        _ => {}
    }
}
