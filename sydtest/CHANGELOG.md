# Changelog

## [0.21.0.0] - 2025-05-09

This is technically a breaking change, but if you are not using the sydtest
constructors directly, it should not break anything for you.

### Added

* `aroundAllWithAll`
* `setupAroundAllWithAll`

### Changed

* Gave the 'DefAroundAllWithNode' constructor access to all outer resources
  instead of just the latest one.

## [0.20.0.1] - 2025-05-09

### Added

* Staged golden tests

## [0.20.0.0] - 2025-05-09

### Added

- `--skip-passed`: Skip passing tests and rerun once the entire suite is
  skipped.

## [0.19.0.0] - 2024-11-17

### Added

- Timeout support
- `modifiedTimeout`
- `withoutTimeout`
- `withTimeout`

### Changed

- Tests now timeout after 60 seconds by default.

## [0.18.0.1] - 2024-11-01

### Changed

- Fixed `mkNotEqualButShouldHaveBeenEqual` logic so it keeps the escape
  sequence for `Text` and `String`. This fix a regression introduced in
  0.18.0.0.

## [0.18.0.0] - 2024-09-26

### Added

- The test `Assertion` which displays a diff in case of error (so `shouldBe`,
  `shouldReturn`, golden tests and variations) will now timeout (after `2s`)
  when computing the diff between expected and actual value.
  In case of timeout, the values are displayed without any diff formatting.
  This ensure that test suite runtime won't be dominated by computing diff on
  some pathological cases.
- The smart constructor `mkNotEqualButShouldHaveBeenEqual` 
- You can use your own diff algorithm using the constructor
  `NotEqualButShouldHaveBeenEqualWithDiff`.
- Test suite does not crash if failed assertion tries to print values
  containing lazy exception.
  For example `shouldBe (1, error "nop") (2, 3)` was crashing before.
  The exception is now reported as the failure reason for the test.
  Note that this can be counter intuitive, because the test is failing because
  values are not equal (e.g. `(1, _) != (2, _)`), and this will be reported
  differently.


### Changed

The diff computation between actual value and reference changed so diff can
timeout.

This does not change the usual API (`shouldBe` or `GoldenTest`), but some
internal changed and you may need to adapt.
The change is straightforward, most of the functions are not `IO`:

- `stringsNotEqualButShouldHaveBeenEqual`,
  `textsNotEqualButShouldHaveBeenEqual` and
  `bytestringsNotEqualButShouldHaveBeenEqual` are now `IO Assertion` (was
  `Assertion`) in order to implement the timeout logic described for
  `shouldBe`.
  The `Assertion` `NotEqualButShouldHaveBeenEqual` is removed and replaced by
  `NotEqualButShouldHaveBeenEqualWithDiff` which embed the difference between
  both values.
- The record field `goldenTestCompare` of `GoldenTest` changed from `a -> a ->
  Maybe Assertion` to `a -> a -> IO (Maybe Assertion)`.

## [0.17.0.2] - 2024-09-26

### Changed

- Sydtest won't crash anymore, behave weirdly, or leak resources when executed
  in a REPL and interrupted by C-c.

## [0.17.0.1] - 2024-09-26

### Changed

* Only use `withArgs` when the argument list isn't already empty.
  This works around a concurrency issue wherein `withArgs` cannot be run twice from multiple threads.

## [0.17.0.0] - 2024-08-04

### Changed

* Allow golden tests to perform IO during comparisons

## [0.16.0.0] - 2024-08-03

### Changed

* `opt-env-conf`-based settings parsing.

## [0.15.1.3] - 2024-07-20

### Changed

* Fix race condition in the asynchronous runner

## [0.15.1.2] - 2024-07-18

### Changed

* Fix parsing filter flags so it becomes easy to select tests with spaces in their description

## [0.15.1.1] - 2023-10-04

### Changed

* Compatibility with `optparse-applicative > 0.18`.
* Compatibility with `GHC >= 9.7`.
* Refactored out `fast-myers-diff` into its own package.

## [0.15.1.0] - 2023-07-28

### Added

* `setupAroundWithAll`: so it's easier to use multiple outer resources to provide an inner resource, without the need of extra type annotation.

## [0.15.0.0] - 2023-04-08

### Added

* `DefBeforeAllWithNode`: so that `beforeAllWith` can be defined in terms of it and have better parallelism properties.
* `DefSetupNode`: so that `beforeAll_` can be defined in terms of it and have better parallelism properties.

## [0.14.0.0] - 2023-04-05

### Added

* Profiling mode, for figuring out why your test suite is slow.
  Use `--profile` to turn it on.

### Changed

* An improved asynchronous test runner.
* Made `--debug` imply `--retries 0`

## [0.13.0.4] - 2023-03-31

### Added

* Allow supplying multiple `--filter`/`--match` flags.
* Add short names `-m` and `-f` for the filter flag options.

## [0.13.0.3] - 2023-03-27

### Added

* Compatibility with `mtl-2.3`

## [0.13.0.2] - 2023-03-24

### Changed

* Improved white space diffing

## [0.13.0.1] - 2023-01-13

### Changed

* Replaced the diffing algorithm by a custom one.
  This has sped up diffing at least 100x and let us get rid of the `Diff` and `split` dependencies.
* Reworded 'likely not flaky' to 'does not look flaky' to be more technically
  accurate as we know nothing about the likelihood of flakiness.

## [0.13.0.0] - 2022-10-14

### Changed

* Changed the default of --golden-start to false.
  If this causes any of your tests to fail, that means that you are not
  correctly shipping the golden results along with your test suite.
* Fixed a typo in the parallelisation warning that you see when you run sydtest in parallel with only one core..

## [0.12.0.1] - 2022-10-06

* Released the changelog with a date.

## [0.12.0.0] - 2022-10-06

### Added

* Automatic flakiness diagnostics, see the `README`
  This adds the `potentiallyFlaky` and `potentiallyFlakyWith` functions.
* `Test.Syd` now also exports `pPrint`.
* The `modifyRetries`, `withoutRetries`, `withRetries` functions, to allow configuration of the number of retries independently of whether flakiness is allowed.
* The `TestRunReport` type, so that a `ResultForest` contains information about all runs of a test instead of only the last.
* Expectation of failure.
  This adds the `expectFailing`, `expectPassing`, and `withExpectationMode` functions.

### Changed

* Fixed: Fail-fast now works correctly together with fail-on-flaky
* Fixed that flags with a `no-` prefix did not parse correctly and could therefore not be used.
* The `FlakinessMode` type no longer contains a number of retries.
  The number of retries is now configured separately.
* Fixed that `xdescribe` would only result in one pending test instead of the same number of tests as are marked as pending.
* Fixed that `specify` and `prop` would show a callstack from inside `sydtest` instead of from where you used them.
* Sydtest now sets the global pseudorandomness seed in the random library to the given seed using `setStdGen` for extra reproducability.

### Removed

## [0.11.0.2] - 2022-09-7

### Changed

* Added the `shouldMatchList` expectation, corresponding the one from
  `hspec-expectations`.

## [0.11.0.1] - 2022-06-28

### Changed

* Made the type of `setupAround` more general.

## [0.11.0.0] - 2022-06-28

### Changed

* `printOutputSpecForest` now uses `Data.Text.Lazy.IO` to output the report, so it will try to respect the system's locale.
* `renderResultReport` now outputs a `Text.Builder` instead of a `ByteString.Builder` and a newline at the end.

## [0.10.1.0] - 2022-06-04

### Changed

* Fixed a bug where multiple layers of contextual failures were not unwrapped and shown correctly.
* Made 'Assertion' fields strict.

## [0.10.0.0] - 2022-04-28

### Added

* Allow supplying filters without a flag, so `hello` instead of `--filter hello` or `--match hello`.
* Simplified the way exceptions work to allow for attaching context to any failure instead of only `Assertion`s.

## [0.9.0.0] - 2022-04-13

### Added

* Optional type-safe progress reporting for property tests.

## [0.8.0.1] - 2022-02-11

### Changed

* Fixed property tests without labels still showing labels in the output.

## [0.8.0.0] - 2022-02-11

### Changed

* The `TestDefM` now contains a `TestDefEnv` which also contains the test description path, along with `TestRunSettings`.
* Removed the `MonadState ()` instance of `TestDefM`. It was just silly.
* Changed the internals of `TestDefM` to use `ReaderT` and a strict `WriterT` instead of `RWST`.
* Renamed `wrapRWST` to `wrapForest`.
* Fixed the property label output to use the right total.
* Moved `filterTestForest` and `randomiseTestForest` from `Test.Syd.TestDefM` to `Test.Syd.SpecDef`, where `SpecDefForest` is defined.

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
