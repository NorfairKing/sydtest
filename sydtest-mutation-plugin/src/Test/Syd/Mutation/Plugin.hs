module Test.Syd.Mutation.Plugin (plugin) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import GHC
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Plugins
import GHC.Driver.Session (WarningFlag (..), wopt_unset)
import GHC.Tc.Types
import System.Environment (lookupEnv)
import System.IO (IOMode (..), hClose, hPutStrLn, openFile)
import Test.Syd.Mutation.Plugin.Instrument
import Test.Syd.Mutation.Runtime (MutationId (..))

plugin :: Plugin
plugin =
  defaultPlugin
    { -- We instrument at the typechecked stage (GhcTc) so that mutations can
      -- be type-directed (e.g. replace any expression of type 'Maybe a' with
      -- 'Nothing', or only mutate '+' when the operands are numeric).
      typeCheckResultAction = mutationTypeCheckAction,
      -- Add an import of Test.Syd.Mutation.Plugin.Runtime at the parsed stage so that
      -- ifMutation and MutationId are in tcg_rdr_env for the typecheck action.
      parsedResultAction = mutationAddRuntimeImport,
      -- Suppress -Wunused-imports for the injected import of Test.Syd.Mutation.Plugin.Runtime.
      driverPlugin = \_ hscEnv ->
        pure hscEnv {hsc_dflags = wopt_unset (hsc_dflags hscEnv) Opt_WarnUnusedImports},
      pluginRecompile = impurePlugin
    }

-- | Inject @import Test.Syd.Mutation.Plugin.Runtime ()@ into every instrumented module.
-- This ensures sydtest-mutation-plugin is registered as used (it is already in
-- build-depends as the plugin package), and satisfies -Wunused-packages.
mutationAddRuntimeImport ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
mutationAddRuntimeImport opts ms pr = do
  let mn = moduleNameString (moduleName (ms_mod ms))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure pr
    else do
      let pm = parsedResultModule pr
          lm = hpm_module pm
          runtimeImport = noLocA (simpleImportDecl (mkModuleName "Test.Syd.Mutation.Plugin.Runtime"))
          lm' = fmap (\m -> m {hsmodImports = runtimeImport : hsmodImports m}) lm
      pure pr {parsedResultModule = pm {hpm_module = lm'}}

mutationTypeCheckAction ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
mutationTypeCheckAction opts _ms tcGblEnv = do
  let mn = moduleNameString (moduleName (tcg_mod tcGblEnv))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  let manifestPath = mapMaybe (stripPrefix "--manifest=") opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure tcGblEnv
    else do
      liftIO $ putStrLn $ "mutation: instrumenting " ++ mn
      (binds', mutations) <-
        runInstrument tcGblEnv $
          instrumentModule (tcg_binds tcGblEnv)
      -- The manifest path comes from --manifest= plugin opt, or from the
      -- MUTATION_MANIFEST env var (used by the Nix build so the store path
      -- can be passed without shell expansion in configureFlags).
      envPath <- liftIO $ lookupEnv "MUTATION_MANIFEST"
      let resolvedPath = case manifestPath of
            (p : _) -> Just p
            [] -> envPath
      liftIO $ case resolvedPath of
        Nothing -> mapM_ (\r -> putStrLn $ "mutation: " ++ renderMutationId (mutRecId r)) mutations
        Just path -> appendManifest path mutations
      pure tcGblEnv {tcg_binds = binds'}

-- | Append mutation records to the manifest file, retrying on lock contention.
-- GHC may compile modules in parallel (-j); we retry on "resource busy" so
-- that concurrent plugin invocations do not race on the same file.
appendManifest :: FilePath -> [MutationRecord] -> IO ()
appendManifest path mutations = retryOnBusy 200 $ do
  h <- openFile path AppendMode
  mapM_ (hPutStrLn h . renderMutationId . mutRecId) mutations
  hClose h
  where
    retryOnBusy :: Int -> IO () -> IO ()
    retryOnBusy 0 action = action
    retryOnBusy n action =
      action `catch` \e -> do
        let isBusy = "resource busy" `isInfixOf` show (e :: IOException)
        if isBusy
          then do
            threadDelay 10000 -- 10ms
            retryOnBusy (n - 1) action
          else throwIO e

-- | Render a 'MutationId' as a tab-separated line for the manifest file.
renderMutationId :: MutationId -> String
renderMutationId (MutationId parts) = intercalate "\t" parts
