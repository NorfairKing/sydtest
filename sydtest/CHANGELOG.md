# Changelog

## Unreleased: [0.8.0.0]

### Changed

* The `TestDefM` now contains a `TestDefEnv` which also contains the test description path, along with `TestRunSettings`.
* Removed the `MonadState ()` instance of `TestDefM`. It was just silly.
* Changed the internals of `TestDefM` to use `ReaderT` and a strict `WriterT` instead of `RWST`.
* Renamed `wrapRWST` to `wrapForest`.
* Fixed the property label output to use the right total.

### Added

* `getTestDescriptionPath` to get the test description path upwards from inside a test definition.


## [0.7.0.1] - 2021-12-23

### Changed

* Fixed `shouldStartWith` to test on the prefix rather then infix

## [0.7.0.0] - 2021-12-15

### Added

* Flaky tests now show up in the failure report when `--fail-on-flaky` is active.
* Flakiness information like the number of retries is now shown in the failure report for real (non-flaky) failures.

### Changed

* Simplified the way settings are passed around.

## [0.6.1.0] - 2021-12-10

### Added

* The `flakyWith` combinator, which is like `flaky`, but lets you also add a message to your team.

## [0.6.0.0] - 2021-11-12

### Changed

* Started using `autodocodec` instead of `yamlparse-applicative`.

## [0.5.0.0] - 2021-11-12

### Added

* The flakiness combinators (`flaky`, `notFlaky`, and `withFlakiness`) to mark a test group as potentially flaky.
* The `--fail-on-flaky` flag to falsify flakiness.
* Experimental Windows support

### Changed

* Fixed the interpretation of `max-size` vs `max-success` in the configuration file and environment parsing.

## [0.4.1.0] - 2021-10-10

### Added

* The `--random-seed` option to use random seeds instead of the fixed seed that is used by default.

## [0.4.0.0] - 2021-09-02

### Added

* The `--debug` option.

### Changed

* Redid the entire flags parsing.
  This should be backward compatible, and result in a nicer `--help` overview.

## [0.3.0.3] - 2021-08-07

### Changed

* Show the total number of examples in the output as well

## [0.3.0.2] - 2021-07-06

### Changed

* Accept options using American spelling as well.

## [0.3.0.1] - 2021-06-20

### Changed

* Turned off shrinking when using `around` and friends. See https://github.com/nick8325/quickcheck/issues/331.

## [0.3.0.0] - 2021-06-17

### Added

* An `IsTest (ReaderT env IO a)` instance.

### Deleted

* `Test.Syd.Def.Env`, which contained `eit` and `withTestEnv`
  Now that `ReaderT env IO a` is also in `IsTest`, you can just use `it` for this.

## [0.2.0.0] - 2021-06-03

### Added

* `beforeWith` and `beforeWith'`
* `scenarioDir` and `scenarioDirRecur` for scenario testing.
* `bracketSetupFunc`

### Changed

* The `SetupFunc` has been simplified to only take one type parameter.

### Deleted

* `composeSetupFunc`, now obsolete: use `<=<` instead.
* `connectSetupFunc`, now obsolete: use `>=>` instead.
* `wrapSetupFunc`, now entirely obsolete.
* `unwrapSetupFunc`, now entirely obsolete.
* `makeSimpleSetupFunc`, now obsolete: Use the `SetupFunc` constructor directly.
* `useSimpleSetupFunc`, now obsolete: Use the `unSetupFunc` function directly.

## [0.1.0.0] - 2021-03-07

Various fixes

## [0.0.0.0] - 2020-12-26

Initial release
