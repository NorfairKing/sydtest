# Changelog

## [0.2.0.0] - 2026-10-09

### Changed

- `sqitchPersistentPostgresqlSpec` now declares one test per change in
  the plan (following the change in `sydtest-sqitch-postgres` 0.2.0.0),
  so the per-test timeout is a budget for one change instead of for the
  whole plan. Test names under `sqitch sanity checks` change
  accordingly, which matters if you filter on them.

## [0.1.0.0] - 2026-06-27

### Changed

- Both the persistent baseline and the sqitch deploy now run in a fresh,
  randomly-named non-`public` schema (following the change in
  `sydtest-sqitch-postgres` 0.1.0.0), so migrations that hardcode a
  schema name are caught by the schema-equality check too.

## [0.0.0.0] - 2026-05-17

First release.
