//@edition: 2018
//@check-pass

fn main() {
    match 42 {
        0...1 => {}
        _ => {}
    }
}
