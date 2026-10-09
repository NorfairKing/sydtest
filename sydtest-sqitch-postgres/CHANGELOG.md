# Changelog

## [0.1.1.0] - 2026-10-09

### Added

- `withPlanScaledTimeout`, which multiplies the per-test timeout by the
  number of changes in the plan, and `readSqitchPlanSteps` to read them.
  `sqitchPostgresqlSpec` now uses both, so a budget that used to cover
  the whole plan at once covers one change.

## [0.1.0.0] - 2026-06-27

### Changed

- Migrations are now deployed into a fresh, randomly-named non-`public`
  schema instead of `public`, created and dropped (`CASCADE`) via a
  `SetupFunc` so tests leave nothing behind. This surfaces migrations
  that hardcode a schema name (e.g. a deploy guard or verify with
  `table_schema = 'public'`), which previously passed unnoticed because
  the tests ran in `public`. Schema snapshots now follow
  `current_schema()`.
- **Breaking:** `sqitchTargetFromOptions` now takes the target schema as
  its first argument; it is carried on `SqitchTarget` and applied to
  sqitch's connections via `PGOPTIONS`.

### Added

- `randomSchemaSetupFunc`, `randomSchemaName`, `schemaSetupFunc`, and
  `useTestSchema` for managing the per-test schema. `randomSchemaSetupFunc`
  generates a fresh schema name and supplies it; `schemaSetupFunc` takes a
  given name (used when the same schema must be shared across databases).

## [0.0.0.0] - 2026-05-17

First release.
