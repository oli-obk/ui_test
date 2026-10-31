#![allow(long_running_const_eval)]

fn main() {
    const {
        let mut i = 0;
        // This should take at least 1s to build. Increase the number if a computer is too fast
        // and finishes building in less than 1s.
        while i < 10_000_000 {
            i += 1;
        }
    };
}
