//! Default argument processing when `ui_test` is used
//! as a test driver.

use std::num::NonZeroUsize;

use color_eyre::eyre::{bail, eyre, Result};

/// Plain arguments if `ui_test` is used as a binary.
#[derive(Debug)]
pub struct Args {
    /// Filters that will be used to match on individual tests
    pub filters: Vec<String>,

    /// Whether to minimize output given to the user.
    pub quiet: bool,

    /// Whether error on mismatches between `.stderr` files and actual
    /// output. Will update the files otherwise.
    pub check: bool,

    /// The number of threads to use
    pub threads: NonZeroUsize,
}

impl Args {
    /// Arguments if `ui_test` is used as a `harness=false` test, called from `cargo test`.
    pub fn test() -> Result<Self> {
        let mut args = Args {
            filters: vec![],
            quiet: false,
            check: false,
            threads: match std::env::var_os("RUST_TEST_THREADS") {
                None => std::thread::available_parallelism()?,
                Some(n) => n
                    .to_str()
                    .ok_or_else(|| eyre!("could not parse RUST_TEST_THREADS env var"))?
                    .parse()?,
            },
        };
        for arg in std::env::args().skip(1) {
            if arg == "--" {
                continue;
            }
            if arg == "--quiet" {
                args.quiet = true;
            } else if arg == "--check" {
                args.check = true;
            } else if arg == "--help" {
                bail!("available flags: --quiet, --check, --test-threads=n")
            } else if let Some(n) = arg.strip_prefix("--test-threads=") {
                args.threads = n.parse()?;
            } else if arg.starts_with("--") {
                bail!("unknown command line flag `{arg}`");
            } else {
                args.filters.push(arg);
            }
        }
        Ok(args)
    }
}
