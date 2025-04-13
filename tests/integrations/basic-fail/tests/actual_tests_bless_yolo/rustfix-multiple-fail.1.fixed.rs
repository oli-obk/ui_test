pub fn f(_: i32) -> &'static i32 {
    //~^ ERROR: missing lifetime
    unimplemented!()
}

fn main() {
    f(1);
}
