//@aux-build:derive_proc_macro.rs:proc-macro

#[macro_use]
extern crate derive_proc_macro;

fn main() {
    let x = Foo;
    x.bar();
}

#[derive(Something)]
struct Foo;
//~^ ERROR: no method
