pub fn f() -> usize {
    1
}

#[allow(long_running_const_eval)]
const SLOW: () = {
    let mut i = 0;
    while i < 5_000_000 {
        i += 1;
    }
};

pub fn g() {
    f() //~ ERROR: mismatched types
}

fn main() {
    SLOW;
}