# Changelog

## [0.2.0.0] - 2024-08-04

### Changed

* Compatibility with `sydtest >=0.17`

## [0.1.0.0] - 2022-04-28

### Changed

* Golden test now don't write a file if constructing the value fails.
  This prevents having to work around empty files when a `ToJSON` implementation has `undefined` somewhere
