# Changelog

## [0.2.0.0] - 2026-10-01

### Removed

* `runFoundFailure`.  It existed only because a timed-out mutation could
  abort a fail-fast run that the driver then reported as finished, and a
  timeout no longer stops a run at all.


## [0.1.2.0] - 2026-10-01

### Added

* `runSoundness`, for whether a run measured what it set out to measure, and
  `runFoundFailure`, for whether it found anything that fails it.


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
