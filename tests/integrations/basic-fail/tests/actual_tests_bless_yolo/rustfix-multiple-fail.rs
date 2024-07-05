pub fn f(_: i32) -> &i32 {
    //~^ ERROR: missing lifetime
    unimplemented!()
}

fn main() {
    f(1);
}
