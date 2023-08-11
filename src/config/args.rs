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

    /// Whether error on mismatches between `.stderr` files and actual
    /// output. Will update the files otherwise.
    pub check: bool,

    /// The number of threads to use
    pub threads: NonZeroUsize,

    /// Skip tests whose names contain any of these entries.
    pub skip: Vec<String>,
}

impl Args {
    /// Arguments if `ui_test` is used as a `harness=false` test, called from `cargo test`.
    pub fn test() -> Result<Self> {
        let mut args = Args {
            filters: vec![],
            quiet: false,
            check: false,
            skip: vec![],
            threads: match std::env::var_os("RUST_TEST_THREADS") {
                None => std::thread::available_parallelism()?,
                Some(n) => n
                    .to_str()
                    .ok_or_else(|| eyre!("could not parse RUST_TEST_THREADS env var"))?
                    .parse()?,
            },
        };
        let mut iter = std::env::args().skip(1);
        while let Some(arg) = iter.next() {
            if arg == "--" {
                continue;
            }
            if arg == "--quiet" {
                args.quiet = true;
            } else if arg == "--check" {
                args.check = true;
            } else if let Some(skip) = parse_value("--skip", &arg, &mut iter)? {
                args.skip.push(skip.into_owned());
            } else if arg == "--help" {
                bail!("available flags: --quiet, --check, --test-threads=n, --skip")
            } else if let Some(n) = parse_value("--test-threads", &arg, &mut iter)? {
                args.threads = n.parse()?;
            } else if arg.starts_with("--") {
                bail!(
                    "unknown command line flag `{arg}`: {:?}",
                    std::env::args().collect::<Vec<_>>()
                );
            } else {
                args.filters.push(arg);
            }
        }
        Ok(args)
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
