use basic::add;

//@run

fn main() {
    println!(
        "{}{}",
        add(20, 22),
        std::env::var("CLAUDECODE").unwrap_or_default()
    );
}
