{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Persistent.Postgresql.RedundantIndicesSpec (spec) where

import Database.Persist.Sql
import Test.Syd
import Test.Syd.Persistent.Postgresql
import Test.Syd.Persistent.Postgresql.RedundantIndices

spec :: Spec
spec = persistPostgresqlSpec (pure ()) $
  describe "shouldHaveNoRedundantIndices" $ do
    it "passes on an empty database" $ \pool ->
      runPostgresqlTest pool shouldHaveNoRedundantIndices

    it "passes for a primary key and an index on another column" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (id int PRIMARY KEY, a int)" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
        shouldHaveNoRedundantIndices

    it "reports both of two indices on the same columns" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE INDEX t_a_b_1 ON t (a, b)" []
        rawExecute "CREATE INDEX t_a_b_2 ON t (a, b)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a_b_1 ON public.t USING btree (a, b)",
                                  "  is covered by CREATE INDEX t_a_b_2 ON public.t USING btree (a, b)",
                                  "",
                                  "CREATE INDEX t_a_b_2 ON public.t USING btree (a, b)",
                                  "  is covered by CREATE INDEX t_a_b_1 ON public.t USING btree (a, b)"
                                ]
                            )
                      )

    it "reports an index on the leading column of a unique constraint" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, CONSTRAINT t_unique UNIQUE (a, b))" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_unique ON public.t USING btree (a, b)"
                                ]
                            )
                      )

    it "reports an index on the leading columns of another index" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, c int)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        rawExecute "CREATE INDEX t_a_b_c ON t (a, b, c)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a_b ON public.t USING btree (a, b)",
                                  "  is covered by CREATE INDEX t_a_b_c ON public.t USING btree (a, b, c)"
                                ]
                            )
                      )

    it "reports an index once with every index that covers it" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, c int)" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        rawExecute "CREATE INDEX t_a_c ON t (a, c)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a)",
                                  "  is covered by CREATE INDEX t_a_b ON public.t USING btree (a, b)",
                                  "  is covered by CREATE INDEX t_a_c ON public.t USING btree (a, c)"
                                ]
                            )
                      )

    it "passes for an index on the trailing column of another index" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE INDEX t_b ON t (b)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "passes for indices on the same columns of different tables" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int)" []
        rawExecute "CREATE TABLE u (a int)" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
        rawExecute "CREATE INDEX u_a ON u (a)" []
        shouldHaveNoRedundantIndices

    it "passes for a unique index on the leading column of a longer one" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE UNIQUE INDEX t_a ON t (a)" []
        rawExecute "CREATE UNIQUE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "reports a non-unique index on the same columns as a unique one" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int)" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
        rawExecute "CREATE UNIQUE INDEX t_a_unique ON t (a)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_a_unique ON public.t USING btree (a)"
                                ]
                            )
                      )

    it "reports a unique index on the same columns as a unique constraint" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, CONSTRAINT t_unique UNIQUE (a))" []
        rawExecute "CREATE UNIQUE INDEX t_a ON t (a)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE UNIQUE INDEX t_a ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_unique ON public.t USING btree (a)",
                                  "",
                                  "CREATE UNIQUE INDEX t_unique ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_a ON public.t USING btree (a)"
                                ]
                            )
                      )

    it "reports a deferred unique constraint only as covered by an immediate one" $ \pool -> do
      runPostgresqlTest pool $
        rawExecute "CREATE TABLE t (a int, CONSTRAINT t_deferred UNIQUE (a) DEFERRABLE, CONSTRAINT t_immediate UNIQUE (a))" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE UNIQUE INDEX t_deferred ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_immediate ON public.t USING btree (a)"
                                ]
                            )
                      )

    it "reports a unique index with distinct NULLs only as covered by one without" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int)" []
        rawExecute "CREATE UNIQUE INDEX t_distinct ON t (a)" []
        rawExecute "CREATE UNIQUE INDEX t_not_distinct ON t (a) NULLS NOT DISTINCT" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE UNIQUE INDEX t_distinct ON public.t USING btree (a)",
                                  "  is covered by CREATE UNIQUE INDEX t_not_distinct ON public.t USING btree (a) NULLS NOT DISTINCT"
                                ]
                            )
                      )

    it "reports a partial index covered by one with the same predicate" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE INDEX t_a ON t (a) WHERE b IS NULL" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b) WHERE b IS NULL" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a) WHERE (b IS NULL)",
                                  "  is covered by CREATE INDEX t_a_b ON public.t USING btree (a, b) WHERE (b IS NULL)"
                                ]
                            )
                      )

    it "passes for indices with different predicates" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE INDEX t_a ON t (a) WHERE b IS NULL" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "reports both of two indices on the same expression" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a text)" []
        rawExecute "CREATE INDEX t_lower_1 ON t (lower(a))" []
        rawExecute "CREATE INDEX t_lower_2 ON t (lower(a))" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_lower_1 ON public.t USING btree (lower(a))",
                                  "  is covered by CREATE INDEX t_lower_2 ON public.t USING btree (lower(a))",
                                  "",
                                  "CREATE INDEX t_lower_2 ON public.t USING btree (lower(a))",
                                  "  is covered by CREATE INDEX t_lower_1 ON public.t USING btree (lower(a))"
                                ]
                            )
                      )

    it "passes for an index on a column and one on an expression over it" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a text)" []
        rawExecute "CREATE INDEX t_a ON t (a)" []
        rawExecute "CREATE INDEX t_lower ON t (lower(a))" []
        shouldHaveNoRedundantIndices

    it "passes for indices with different operator classes" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a text, b int)" []
        rawExecute "CREATE INDEX t_a ON t (a text_pattern_ops)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "passes for indices with different collations" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a text, b int)" []
        rawExecute "CREATE INDEX t_a ON t (a COLLATE \"C\")" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "passes for indices with different orderings" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, c int)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b DESC)" []
        rawExecute "CREATE INDEX t_a_b_c ON t (a, b, c)" []
        shouldHaveNoRedundantIndices

    it "reports both of two hash indices on the same column" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int)" []
        rawExecute "CREATE INDEX t_a_1 ON t USING hash (a)" []
        rawExecute "CREATE INDEX t_a_2 ON t USING hash (a)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a_1 ON public.t USING hash (a)",
                                  "  is covered by CREATE INDEX t_a_2 ON public.t USING hash (a)",
                                  "",
                                  "CREATE INDEX t_a_2 ON public.t USING hash (a)",
                                  "  is covered by CREATE INDEX t_a_1 ON public.t USING hash (a)"
                                ]
                            )
                      )

    it "passes for indices with different access methods" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int)" []
        rawExecute "CREATE INDEX t_a_hash ON t USING hash (a)" []
        rawExecute "CREATE INDEX t_a_btree ON t (a)" []
        shouldHaveNoRedundantIndices

    it "passes for a leading part of the key of an index that is not a btree" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a box, b box)" []
        rawExecute "CREATE INDEX t_a ON t USING gist (a)" []
        rawExecute "CREATE INDEX t_a_b ON t USING gist (a, b)" []
        shouldHaveNoRedundantIndices

    it "passes for an index that includes a column the longer one lacks" $ \pool ->
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, c int)" []
        rawExecute "CREATE INDEX t_a ON t (a) INCLUDE (c)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        shouldHaveNoRedundantIndices

    it "reports an index whose included column the longer one carries" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int, c int)" []
        rawExecute "CREATE INDEX t_a ON t (a) INCLUDE (c)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b) INCLUDE (c)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a) INCLUDE (c)",
                                  "  is covered by CREATE INDEX t_a_b ON public.t USING btree (a, b) INCLUDE (c)"
                                ]
                            )
                      )

    it "does not count an included column as part of the key" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a int, b int)" []
        rawExecute "CREATE INDEX t_a_b ON t (a, b)" []
        rawExecute "CREATE INDEX t_a ON t (a) INCLUDE (b)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING btree (a) INCLUDE (b)",
                                  "  is covered by CREATE INDEX t_a_b ON public.t USING btree (a, b)"
                                ]
                            )
                      )

    it "does not report the index of an exclusion constraint" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE TABLE t (a box, CONSTRAINT t_exclusion EXCLUDE USING gist (a WITH &&))" []
        rawExecute "CREATE INDEX t_a ON t USING gist (a)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON public.t USING gist (a)",
                                  "  is covered by CREATE INDEX t_exclusion ON public.t USING gist (a)"
                                ]
                            )
                      )

    it "reports indices outside of the public schema" $ \pool -> do
      runPostgresqlTest pool $ do
        rawExecute "CREATE SCHEMA other" []
        rawExecute "CREATE TABLE other.t (a int, b int)" []
        rawExecute "CREATE INDEX t_a ON other.t (a)" []
        rawExecute "CREATE INDEX t_a_b ON other.t (a, b)" []
      runPostgresqlTest pool shouldHaveNoRedundantIndices
        `shouldThrow` ( ==
                          ExpectationFailed
                            ( unlines
                                [ "Found indices that other indices cover:",
                                  "",
                                  "CREATE INDEX t_a ON other.t USING btree (a)",
                                  "  is covered by CREATE INDEX t_a_b ON other.t USING btree (a, b)"
                                ]
                            )
                      )
