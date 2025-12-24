# Changelog

## [0.3.0.0] - 2025-12-26

### Changed

* Complete overhaul: Tests now run with their own database based on a template
  database instead of their own entire postgres server.

## [0.2.0.3] - 2023-10-09

### Added

* Compatibility with `GHC >= 9.8`.

## [0.2.0.2] - 2022-05-05

### Added

* `postgresqlMigrationSucceedsSpec`

## [0.2.0.1] - 2022-04-26

### Added

* Compatibility with `GHC >= 9`

## [0.2.0.0] - 2021-06-19

### Added

* Dependency on `sydtest-persistent`

### Changed

* Type of `runSqliteTest` is changed to run `SqlPersistM` instead of `SqlPersistT IO`
