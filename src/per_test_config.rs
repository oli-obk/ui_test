//! This module allows you to configure the default settings for all
//! tests. All data structures here are normally parsed from `@` comments
//! in the files. These comments still overwrite the defaults, although
//! some boolean settings have no way to disable them.

use std::path::Path;

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

    pub fn collect<'a, T, I: Iterator<Item = T>, R: FromIterator<T>>(
        &'a self,
        f: impl Fn(&'a Revisioned) -> I,
    ) -> R {
        self.comments
            .for_revision(self.revision)
            .flat_map(f)
            .collect()
    }
}
