{-# LANGUAGE OverloadedStrings #-}

-- | Shelling out to @sqitch@ for tests.
module Test.Syd.Sqitch.Process
  ( SqitchSettings (..),
    SqitchTarget,
    sqitchTargetFromTemplateDB,
    sqitchAt,
    sqitchDeployTo,
    sqitchRevertTo,
    sqitchRevertAll,
  )
where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (getLast)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Database.PostgreSQL.Simple.Options as Postgres
import Database.Postgres.Temp (toConnectionOptions)
import Network.URI (escapeURIString, isUnreserved)
import Path
import System.Environment (getEnvironment)
import System.Process.Typed
import Test.Syd.Persistent.Postgresql (TemplateDB)

-- | Static configuration for a sqitch-based test suite.
data SqitchSettings = SqitchSettings
  { -- | Directory containing @sqitch.plan@, @deploy/@, @revert/@, @verify/@.
    sqitchSettingsProjectDir :: Path Abs Dir,
    -- | Absolute path to the sqitch executable. In a Nix-built test binary
    -- this is typically a @\/nix\/store@ path to @sqitch-pg@.
    sqitchSettingsBin :: Path Abs File,
    -- | Sqitch tag name (without the leading @\@@) marking the cut-over
    -- point for idempotence. Every change in plan order at or before the
    -- tag is exempt from the idempotence check (because it shipped to
    -- production before that check existed); every change after the tag
    -- must be idempotent. 'Nothing' means every change must be idempotent.
    sqitchSettingsGrandfatherTag :: Maybe Text
  }

-- | A @sqitch --target@ value: a libpq-style URI pointing at a database.
newtype SqitchTarget = SqitchTarget {unSqitchTarget :: String}

-- | Build a sqitch target URI for a temporary database created by
-- 'Test.Syd.Persistent.Postgresql.persistPostgresqlSpec'. The current
-- database name is queried from the pool by the caller and passed in.
sqitchTargetFromTemplateDB ::
  TemplateDB ->
  -- | Current database name (as reported by @SELECT current_database()@).
  Text ->
  SqitchTarget
sqitchTargetFromTemplateDB (db, (testuser, testpassword, _templatedb)) testdb =
  let options = toConnectionOptions db
      rawHost :: String
      rawHost = fromMaybe "localhost" (getLast (Postgres.host options))
      port :: Word16
      port = maybe 5432 fromIntegral (getLast (Postgres.port options))
      -- A unix-socket directory (which starts with '/') must be
      -- URL-encoded for sqitch's URI parser to keep it intact.
      encodedHost
        | "/" `isPrefixOf` rawHost = escapeURIString isUnreserved rawHost
        | otherwise = rawHost
      enc = escapeURIString isUnreserved
   in SqitchTarget $
        concat
          [ "db:pg://",
            enc (Text.unpack testuser),
            ":",
            enc (Text.unpack testpassword),
            "@",
            encodedHost,
            ":",
            show port,
            "/",
            enc (Text.unpack testdb)
          ]

-- | Run a sqitch subcommand against a target. Forces @TZ=UTC@: sqitch
-- records registry timestamps via Perl @DateTime@, whose local-tz probe
-- can fail nondeterministically and cause it to roll back a successful
-- deploy.
sqitchAt ::
  SqitchSettings ->
  SqitchTarget ->
  -- | Subcommand, e.g. @\"deploy\"@.
  String ->
  -- | Extra arguments.
  [String] ->
  IO ()
sqitchAt settings target cmd extraArgs = do
  env <- getEnvironment
  runProcess_ $
    setEnv (("TZ", "UTC") : filter ((/= "TZ") . fst) env) $
      setWorkingDir (fromAbsDir (sqitchSettingsProjectDir settings)) $
        proc (fromAbsFile (sqitchSettingsBin settings)) $
          [cmd, "--target", unSqitchTarget target] <> extraArgs

-- | @sqitch deploy --target ... --to <change> --verify@
sqitchDeployTo :: SqitchSettings -> SqitchTarget -> Text -> IO ()
sqitchDeployTo settings target change =
  sqitchAt settings target "deploy" ["--to", Text.unpack change, "--verify"]

-- | @sqitch revert --target ... --to <change> -y@
sqitchRevertTo :: SqitchSettings -> SqitchTarget -> Text -> IO ()
sqitchRevertTo settings target change =
  sqitchAt settings target "revert" ["--to", Text.unpack change, "-y"]

-- | @sqitch revert --target ... -y@
sqitchRevertAll :: SqitchSettings -> SqitchTarget -> IO ()
sqitchRevertAll settings target =
  sqitchAt settings target "revert" ["-y"]
