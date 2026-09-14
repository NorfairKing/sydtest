# Changelog

## [0.3.0.0] - 2026-09-14

### Added

* Both phases write a timing report next to their output: `timing.html` and
  `timing.json`, with an abridged copy on stdout.  It says where the run's
  time went - per child split into process startup, suite setup and test
  execution, and grouped by suite, outcome, operator, module and mutation -
  so a run that takes hours can be attributed instead of guessed at.  The
  page sorts the whole per-child listing by any column and filters it, which
  is what a run with thousands of mutations needs and a build log cannot
  give.  The mutation phase writes to its report directory, the coverage
  phase to its augmented-manifest directory.
* `assert-score` links the timing files into its output directory and names
  `timing.html` alongside the report paths.

### Changed

* `runCoverageMode` takes a resolved job count rather than a `Maybe Word`, and
  returns the per-test timings for the caller to accumulate across suites.

## [0.2.0.0] - 2026-09-11

### Fixed

* The output of the run that killed a control is kept, so the flaky test behind
  a control failure can be found.



## [0.1.0.0] - 2026-07-16

* First released version.
