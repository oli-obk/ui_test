//! Default argument processing when `ui_test` is used
//! as a test driver.

/// Plain arguments if `ui_test` is used as a binary.
pub struct Args {
    /// Filters that will be used to match on individual tests
    pub filters: Vec<String>,

    /// Whether to minimize output given to the user.
    pub quiet: bool,

    /// Whether error on mismatches between `.stderr` files and actual
    /// output. Will update the files otherwise.
    pub check: bool,
}

impl Args {
    /// Arguments if `ui_test` is used as a `harness=false` test, called from `cargo test`.
    pub fn test() -> Self {
        let mut args = Args {
            filters: vec![],
            quiet: false,
            check: false,
        };
        for arg in std::env::args().skip(1) {
            if arg == "--" {
                continue;
            }
            if arg == "--quiet" {
                args.quiet = true;
            } else if arg == "--check" {
                args.check = true;
            } else {
                args.filters.push(arg);
            }
        }
        args
    }
}
