//@aux-build:the_proc_macro.rs:proc-macro
//! Test that our automatic --crate-type detection (changing this crate to `lib`)
//! does not change the aux build to `lib`

use the_proc_macro::thing;

thing!(cake);
