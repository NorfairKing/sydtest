{-# LANGUAGE OverloadedStrings #-}

-- | Shelling out to @sqitch@ for tests.
module Test.Syd.Sqitch.Postgresql.Process
  ( SqitchSettings (..),
    SqitchTarget,
    sqitchTargetFromOptions,
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
import Network.URI (escapeURIString, isUnreserved)
import Path
import System.Environment (getEnvironment)
import System.Process.Typed

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

-- | A @sqitch --target@ value: a libpq-style URI pointing at a
-- database, together with the schema sqitch should deploy into. The
-- schema is carried here (rather than baked into the URI) so 'sqitchAt'
-- can put it on @search_path@ via @PGOPTIONS@ for every subcommand.
data SqitchTarget = SqitchTarget
  { sqitchTargetUri :: String,
    sqitchTargetSchema :: Text
  }

-- | Build a sqitch target from libpq connection 'Postgres.Options' and
-- the schema to deploy into. Handles the unix-socket host case (host
-- starts with @/@) by URL-encoding the host segment so sqitch's URI
-- parser keeps the socket path intact.
sqitchTargetFromOptions :: Text -> Postgres.Options -> SqitchTarget
sqitchTargetFromOptions schema options =
  let rawHost :: String
      rawHost = fromMaybe "localhost" (getLast (Postgres.host options))
      port :: Word16
      port = maybe 5432 fromIntegral (getLast (Postgres.port options))
      user :: String
      user = fromMaybe "" (getLast (Postgres.user options))
      password :: String
      password = fromMaybe "" (getLast (Postgres.password options))
      dbname :: String
      dbname = fromMaybe "" (getLast (Postgres.dbname options))
      encodedHost
        | "/" `isPrefixOf` rawHost = escapeURIString isUnreserved rawHost
        | otherwise = rawHost
      enc = escapeURIString isUnreserved
   in SqitchTarget
        { sqitchTargetUri =
            concat
              [ "db:pg://",
                enc user,
                ":",
                enc password,
                "@",
                encodedHost,
                ":",
                show port,
                "/",
                enc dbname
              ],
          sqitchTargetSchema = schema
        }

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
  -- Scope every sqitch connection to the target's schema via search_path,
  -- so its unqualified DDL lands there and current_schema() resolves to
  -- it. public stays on the path for shared functions.
  let overrides =
        [ ("TZ", "UTC"),
          ("PGOPTIONS", "-c search_path=" <> Text.unpack (sqitchTargetSchema target) <> ",public")
        ]
      kept (k, _) = k `notElem` map fst overrides
  runProcess_ $
    setEnv (overrides <> filter kept env) $
      setWorkingDir (fromAbsDir (sqitchSettingsProjectDir settings)) $
        proc (fromAbsFile (sqitchSettingsBin settings)) $
          [cmd, "--target", sqitchTargetUri target] <> extraArgs

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
