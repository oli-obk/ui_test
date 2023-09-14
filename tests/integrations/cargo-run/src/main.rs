fn main() {
    let arg = std::env::args().nth(1).unwrap();
    let input = std::fs::read_to_string(arg).unwrap();
    let mut b = std::io::stdin().lines();
    for (line, a ) in input.lines().enumerate() {
        let b = b.next().expect(&format!(".stdin file is missing line {}", line + 1)).unwrap();
        assert_eq!(a, b);
    }
    for b in b {
        panic!("{}", b.unwrap());
    }
    panic!("done");
}
