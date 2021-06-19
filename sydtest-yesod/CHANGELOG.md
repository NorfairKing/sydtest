# Changelog

## [0.2.0.1] - 2021-06-17

### Changed

* Fixed that the error message for `locationShouldBe` was backwards.

## [0.2.0.0] - 2021-06-17

### Added

* Added `IsTest` instances for `YesodClientM site`

### Changed

* Simplified `yit` using the new `ReaderT` instance in sydtest.
  It now requires the test to return `()`.
