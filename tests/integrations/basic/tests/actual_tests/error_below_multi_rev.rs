//@no-rustfix
//@revisions: edition2015 edition2018
//@[edition2015] edition:2015
//@[edition2018] edition:2018

#![deny(dead_code, unused_imports)]

//~v unused_imports
use ::std::vec;

mod m {
  //~[edition2015]v dead_code
  pub fn foo() {}
}

//~[edition2015]| unused_imports
//~[edition2018]v E0432
use ::m::foo;

fn main() {}
