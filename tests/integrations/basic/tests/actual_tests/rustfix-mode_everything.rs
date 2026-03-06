//@rustfix-mode: everything
#![deny(unused_imports)]
#![warn(non_upper_case_globals)]
#![allow(dead_code)]

use std::collections::HashMap;
//~^ ERROR: unused import

pub static x: u32 = 0;
//~^ WARN: should have an upper case name

fn main() {}
