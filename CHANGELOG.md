# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* Started maintaining a changelog
* `Config::comment_defaults` allows setting `//@` comments for all tests
* `//~` comments can now specify just an error code or lint name, without any message. ERROR level is implied

### Fixed

### Changed

* crate-private span handling was passed off to the `spanned` crate, improving some diagnostics along the way.

### Removed

* `$DIR` and `RUSTLIB` replacements
* `Config::edition` (replaced by `config.comment_defaults.base().edition`)
* `Config::filter_stdout` (replaced by `config.comment_defaults.base().normalize_stdout`)
* `Config::filter_stderr` (replaced by `config.comment_defaults.base().normalize_stderr`)
* `Config::mode` (replaced by `config.comment_defaults.base().mode`)

## [0.21.2] - 2023-09-27
