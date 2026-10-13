# Changelog

## [0.4.1.0] - 2026-10-13

### Changed

* A control (no-op) mutation is checked against ten of its covering tests
  rather than against all of them.


## [0.4.0.1] - 2026-10-13

### Changed

* A mutation child is pointed at a directory holding only the mutation it is
  asked about, instead of at the run's whole augmented manifest.


## [0.4.0.0] - 2026-10-01

### Fixed

* `assert-score` no longer fails on a mutation that ran out of time, and nor
  does a `--fail-fast` run.  Such a mutation is a kill again -- mutating a
  loop's exit condition turns it into `while True`, and a suite that never
  finishes is one that noticed -- so there is nothing left to fail on.  See
  sydtest 0.33.0.0.


## [0.3.0.0] - 2026-10-01

### Changed

* A mutation that only overran its budget is given another go, up to three
  attempts.
* A run with a failed control exits non-zero, so its verdict is not cached
  and the run can simply be retried.
* `assert-score` fails on a mutation that ran out of time, and so does a
  `--fail-fast` run.


## [0.2.0.0] - 2026-09-11

### Fixed

* The output of the run that killed a control is kept, so the flaky test behind
  a control failure can be found.



## [0.1.0.0] - 2026-07-16

* First released version.
