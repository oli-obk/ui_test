pub fn add(left: usize, right: usize) -> usize {
    left + right
}

fn main() {
    add("42", 3);
    //~^ E0308

    add("42", 3);
}
