# Changelog

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
