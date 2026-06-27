# Changelog

## [0.1.0.0] - 2026-06-27

### Changed

- Both the persistent baseline and the sqitch deploy now run in a fresh,
  randomly-named non-`public` schema (following the change in
  `sydtest-sqitch-postgres` 0.1.0.0), so migrations that hardcode a
  schema name are caught by the schema-equality check too.

## [0.0.0.0] - 2026-05-17

First release.
