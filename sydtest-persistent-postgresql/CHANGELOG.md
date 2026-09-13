# Changelog

## [0.5.0.0] - 2026-09-13

### Added

* A suite can run against a real PostgreSQL hot standby of the database it
  writes to, so an application that offloads reads onto a read replica can be
  tested on one. `persistPostgresqlReplicatedSpec` is `persistPostgresqlSpec`
  with a replica, and gives each test a `ReplicatedPools`: a pool to a fresh
  database on the primary, and a pool to the same database on the standby.

  The standby is a `pg_basebackup` streaming through a replication slot, held
  behind by `recovery_min_apply_delay` so that being behind is a fact rather
  than a race. It replays the same write-ahead log a production replica does,
  in the same order, and refuses writes because postgres refuses them.

* `onPrimary`, `onReplica` and `awaitReplica` for reaching either end and for
  waiting until the standby has caught up.

* `unreplicatedPools`, for the application that is configured without a
  replica: the same database through both pools, where `awaitReplica` returns
  at once.

* `ReplicaConfig` and `defaultReplicaConfig` for how far behind the replica is
  held, and `persistPostgresqlReplicatedSpecWith` to choose it.

* `persistPostgresqlReplicatedAdminSpec`, `replicatedPoolsSpec`,
  `postgresqlStandbySetupFunc`, `standbyPoolSetupFunc`,
  `replicatedPoolsSetupFunc`, `replicationPrimaryConfig`, `ReplicatedDB` and
  `Standby`, for a suite that needs the standby somewhere else in its resource
  stack.

* `postgresqlServerSetupFuncWith` and `persistPostgresqlAdminSpecWith`, which
  take the `tmp-postgres` configuration from the caller. `tmp-postgres`'
  `defaultConfig` asks for `wal_level = minimal`, which writes too little to
  the log for anything to replay and at which postgres refuses to start with a
  nonzero `max_wal_senders`, so the configuration had to become the caller's to
  choose.

* `testDatabaseSetupFunc`, which is `connectionPoolSetupFunc` plus the
  connection options the pool was opened with. A second connection to the same
  database, which is what the standby pool is, needs to know which database
  that is.

## [0.4.0.0] - 2026-05-17

### Added

* `postgresqlServerSetupFunc`, `postgresqlUserSetupFunc`, and
  `postgresqlDatabaseSetupFunc` are now exported as building blocks
  for composing custom setup chains.
* `emptyPostgresPoolSetupFunc` provides a `ConnectionPool` to a fresh
  empty database (server + user + db + pool) without going through a
  migrated template.

### Changed

* Internal helpers `adminDBSetupFunc`, `tempUserSetupFunc`, and
  `tempNewDatabaseSetupFunc` were renamed to `postgresqlServerSetupFunc`,
  `postgresqlUserSetupFunc`, and `postgresqlDatabaseSetupFunc` and
  promoted to the public API.

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
