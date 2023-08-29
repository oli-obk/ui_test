//! Default argument processing when `ui_test` is used
//! as a test driver.

use std::{borrow::Cow, num::NonZeroUsize};

use color_eyre::eyre::{bail, ensure, eyre, Result};

/// Plain arguments if `ui_test` is used as a binary.
#[derive(Debug)]
pub struct Args {
    /// Filters that will be used to match on individual tests
    pub filters: Vec<String>,

    /// Whether to minimize output given to the user.
    pub quiet: bool,

    /// Whether to error on mismatches between `.stderr` files and actual
    /// output.
    pub check: bool,

    /// Whether to overwrite `.stderr` files on mismtach with the actual
    /// output.
    pub bless: bool,

    /// The number of threads to use
    pub threads: NonZeroUsize,

    /// Skip tests whose names contain any of these entries.
    pub skip: Vec<String>,
}

impl Args {
    /// Dummy arguments, but with the number of threads loaded from the environment.
    /// The boolearn argument decides whether to bless (`true`) or whether to error (`false`)
    fn default(bless: bool) -> Result<Self> {
        Ok(Args {
            filters: vec![],
            quiet: false,
            bless: bless,
            check: !bless,
            skip: vec![],
            threads: match std::env::var_os("RUST_TEST_THREADS") {
                None => std::thread::available_parallelism()?,
                Some(n) => n
                    .to_str()
                    .ok_or_else(|| eyre!("could not parse RUST_TEST_THREADS env var"))?
                    .parse()?,
            },
        })
    }

    /// Parse the program arguments.
    /// This is meant to be used if `ui_test` is used as a `harness=false` test, called from `cargo test`.
    pub fn test() -> Result<Self> {
        Self::default(true)?.parse_args(std::env::args().skip(1))
    }

    /// Parse arguments into an existing `Args` struct.
    pub fn parse_args(mut self, mut iter: impl Iterator<Item = String>) -> Result<Self> {
        while let Some(arg) = iter.next() {
            if arg == "--" {
                continue;
            }
            if arg == "--quiet" {
                self.quiet = true;
            } else if arg == "--check" {
                self.check = true;
            } else if let Some(skip) = parse_value("--skip", &arg, &mut iter)? {
                self.skip.push(skip.into_owned());
            } else if arg == "--help" {
                bail!("available flags: --quiet, --check, --test-threads=n, --skip")
            } else if let Some(n) = parse_value("--test-threads", &arg, &mut iter)? {
                self.threads = n.parse()?;
            } else if arg.starts_with("--") {
                bail!(
                    "unknown command line flag `{arg}`: {:?}",
                    iter.collect::<Vec<_>>()
                );
            } else {
                self.filters.push(arg);
            }
        }
        Ok(self)
    }
}

fn parse_value<'a>(
    name: &str,
    arg: &'a str,
    iter: &mut impl Iterator<Item = String>,
) -> Result<Option<Cow<'a, str>>> {
    let with_eq = match arg.strip_prefix(name) {
        Some(s) => s,
        None => return Ok(None),
    };
    if let Some(n) = with_eq.strip_prefix('=') {
        Ok(Some(n.into()))
    } else {
        ensure!(with_eq.is_empty(), "`{name}` can only be followed by `=`");

        if let Some(next) = iter.next() {
            Ok(Some(next.into()))
        } else {
            bail!("`name` must be followed by a value")
        }
    }
}
