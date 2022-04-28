# Changelog

## [0.1.0.0] - 2022-04-28

### Changed

* Golden test now don't write a file if constructing the value fails.
  This prevents having to work around empty files when a `ToJSON` implementation has `undefined` somewhere
