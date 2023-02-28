//@aux-build:derive_proc_macro.rs:proc-macro

#[macro_use]
extern crate derive_proc_macro;

fn main() {
    let x = Foo;
    x.bar();
    //~^ ERROR: no method
}

#[derive(Something)]
struct Foo;
