# Changelog

## [0.2.0.0] - Unreleased

### Added

* Added `IsTest` instances for `YesodClientM site`

### Changed

* Simplified `yit` using the new `ReaderT` instance in sydtest.
  It now requires the test to return `()`.
