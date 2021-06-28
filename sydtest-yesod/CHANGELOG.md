# Changelog

## [0.3.0.0] - 2021-06-29

### Added

* End-to-end testing support:

  * `yesodE2ESpec`
  * `yesodE2ESpec'`
  * `E2E (..)`
  * `localToE2ESpec`
  * `localToE2EClient`

### Changed

* A `YesodClient site` now contains a base `URI` instead of a `PortNumber`.
* The types of `getLocation` and `locationShouldBe` are now more general, to support end-to-end testing.


## [0.2.0.1] - 2021-06-17

### Changed

* Fixed that the error message for `locationShouldBe` was backwards.

## [0.2.0.0] - 2021-06-17

### Added

* Added `IsTest` instances for `YesodClientM site`

### Changed

* Simplified `yit` using the new `ReaderT` instance in sydtest.
  It now requires the test to return `()`.
