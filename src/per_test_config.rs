//! This module allows you to configure the default settings for all
//! tests. All data structures here are normally parsed from `@` comments
//! in the files. These comments still overwrite the defaults, although
//! some boolean settings have no way to disable them.

use std::path::Path;
use std::process::Command;

use spanned::Spanned;

use crate::parser::OptWithLine;
pub use crate::parser::{Comments, Condition, Revisioned};
pub use crate::rustc_stderr::Level;
use crate::test_result::Errored;
use crate::{strip_path_prefix, Config, Mode};

pub(crate) struct TestConfig<'a> {
    pub config: Config,
    pub revision: &'a str,
    pub comments: &'a Comments,
    pub path: &'a Path,
}

impl TestConfig<'_> {
    pub fn patch_out_dir(&mut self) {
        // Put aux builds into a separate directory per path so that multiple aux files
        // from different directories (but with the same file name) don't collide.
        let relative = strip_path_prefix(self.path.parent().unwrap(), &self.config.out_dir);

        self.config.out_dir.extend(relative);
    }

    pub fn extension(&self, extension: &str) -> String {
        if self.revision.is_empty() {
            extension.to_string()
        } else {
            format!("{}.{extension}", self.revision)
        }
    }

    pub fn mode(&self) -> Result<Spanned<Mode>, Errored> {
        self.comments.mode(self.revision)
    }

    pub fn edition(&self) -> Result<Option<Spanned<String>>, Errored> {
        self.comments.edition(self.revision)
    }

    pub fn find_one<'a, T: 'a>(
        &'a self,
        kind: &str,
        f: impl Fn(&'a Revisioned) -> OptWithLine<T>,
    ) -> Result<OptWithLine<T>, Errored> {
        self.comments.find_one_for_revision(self.revision, kind, f)
    }

    pub fn all(&self) -> impl Iterator<Item = &'_ Revisioned> {
        self.comments.for_revision(self.revision)
    }

    pub fn collect<'a, T, I: Iterator<Item = T>, R: FromIterator<T>>(
        &'a self,
        f: impl Fn(&'a Revisioned) -> I,
    ) -> R {
        self.comments
            .for_revision(self.revision)
            .flat_map(f)
            .collect()
    }

    pub fn build_command(&self) -> Result<Command, Errored> {
        let TestConfig {
            config,
            revision,
            comments,
            path,
        } = self;
        let mut cmd = config.program.build(&config.out_dir);
        cmd.arg(path);
        if !revision.is_empty() {
            cmd.arg(format!("--cfg={revision}"));
        }
        for arg in comments
            .for_revision(revision)
            .flat_map(|r| r.compile_flags.iter())
        {
            cmd.arg(arg);
        }
        let edition = comments.edition(revision)?;

        if let Some(edition) = edition {
            cmd.arg("--edition").arg(&*edition);
        }

        if let Some(target) = &config.target {
            // Adding a `--target` arg to calls to Cargo will cause target folders
            // to create a target-specific sub-folder. We can avoid that by just
            // not passing a `--target` arg if its the same as the host.
            if !config.host_matches(target) {
                cmd.arg("--target").arg(target);
            }
        }

        // False positive in miri, our `map` uses a ref pattern to get the references to the tuple fields instead
        // of a reference to a tuple
        #[allow(clippy::map_identity)]
        cmd.envs(
            comments
                .for_revision(revision)
                .flat_map(|r| r.env_vars.iter())
                .map(|(k, v)| (k, v)),
        );

        Ok(cmd)
    }
}
