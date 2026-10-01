# Changelog

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
