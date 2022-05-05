CREATe TABLE "person"("id" SERIAL8  PRIMARY KEY UNIQUE,"name" VARCHAR NOT NULL,"age" INT8 NULL);

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Test.Syd.Persistent.PostgresqlSpec.persistPostgresqlSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.
