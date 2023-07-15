//! Default argument processing when `ui_test` is used
//! as a test driver.

use clap::Parser;

/// Plain arguments if `ui_test` is used as a binary.
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// Filters that will be used to match on individual tests
    pub filters: Vec<String>,

    /// Whether to minimize output given to the user.
    #[arg(short, long, default_value_t = false)]
    pub quiet: bool,

    /// Whether error on mismatches between `.stderr` files and actual
    /// output. Will update the files otherwise.
    #[arg(long, default_value_t = false)]
    pub check: bool,
}

impl Default for Args {
    fn default() -> Self {
        Self::parse_from([std::env::current_exe().unwrap()])
    }
}

impl Args {
    /// Arguments if `ui_test` is used as a `cargo test`, so we need
    /// to skip a level of `--` in order to not pick up `cargo`'s test
    /// flags.
    pub fn test() -> Self {
        Args::parse_from(std::env::args().skip_while(|arg| arg != "--"))
    }
}
