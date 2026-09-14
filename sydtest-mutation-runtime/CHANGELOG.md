# Changelog

## [0.1.2.0] - 2026-09-14

### Added

* `Test.Syd.Mutation.Timing`: `ChildTiming`, what a mutation or coverage child
  measures of itself and writes to the file the parent names with
  `--mutation-timing-output`.
* `Test.Syd.Mutation.TimingReport`: the recorded per-child timings of a
  mutation or coverage phase, the aggregation over them, and the terminal
  rendering - where a run's time goes, split per child into process startup,
  suite setup and test execution, and grouped by suite, outcome, operator,
  module and mutation.
* `Test.Syd.Mutation.TimingReport.Html`: the same summary as a standalone
  page, with the whole per-child listing sortable by any column and
  filterable, rather than the worst few rows a build log has room for.  No
  external assets, so it opens straight from a Nix store path.

## [0.1.1.0] - 2026-07-29

### Fixed

* `writeManifestTxtFile` now writes UTF-8.  It wrote through a binary handle
  with `hPutStr`, which truncates every `Char` to its low byte, so any
  non-ASCII character in a rendered source line came out as a different
  character (`…` as `&`) or as an invalid UTF-8 byte.  The `.json` manifest
  was unaffected, as is the driver's `report.txt`, which already encoded
  explicitly.

## [0.1.0.0] - 2026-07-16

* First released version.
