{-# LANGUAGE OverloadedStrings #-}

-- | Parsing a @sqitch.plan@ file into a sequence of deploy steps.
module Test.Syd.Sqitch.Postgresql.Plan
  ( PlanStep (..),
    readSqitchPlan,
  )
where

import qualified Data.ByteString as SB
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Path

-- | One step in a sqitch plan: enough information to drive @sqitch deploy@,
-- find the deploy script on disk, identify the step in test output, and
-- decide whether the idempotence check applies.
data PlanStep = PlanStep
  { -- | Human label used as test 'context'.
    stepLabel :: Text,
    -- | Argument to @sqitch deploy --to@ to bring the database to the
    -- post-step state.
    stepDeployTarget :: Text,
    -- | Filename (without directory) in @deploy/@, e.g. @init.sql@ or
    -- @add-foo\@v2026-01-01.sql@.
    stepScriptName :: Text,
    -- | If 'True', skip the per-change round-trip /and/ idempotence
    -- checks for this step. Set when the step is at or before the
    -- grandfather tag (see 'readSqitchPlan').
    --
    -- These checks are end-state invariants that grandfathered scripts
    -- aren't expected to satisfy in isolation -- they shipped before the
    -- checks existed, and the failure modes they guard against (revert
    -- drift, registry retry) can't manifest on databases that already
    -- ran them. The whole-plan cycle and schema-equality checks still
    -- apply to grandfathered steps end-to-end.
    stepIsGrandfathered :: Bool,
    -- | If 'True', this step is the post-tag head of a reworked change
    -- (its deploy target is @name\@HEAD@). The round-trip check is
    -- skipped on such steps because sqitch's revert of just the rework
    -- runs the rework's revert script, which by sqitch convention
    -- undoes the whole change rather than only the rework. A partial
    -- revert/redeploy therefore does not land at post(rework) and would
    -- fail the round-trip check spuriously. Whole-plan deploy/revert
    -- cycles still exercise this code path.
    stepIsReworkHead :: Bool
  }
  deriving (Show, Eq)

-- | Parse a @sqitch.plan@ file into one 'PlanStep' per change.
--
-- The optional grandfather tag marks a cut-over point: every change in
-- plan order at or before the tag is marked 'stepIsGrandfathered = True';
-- every change after it is 'False'. Pass 'Nothing' to mean \"no
-- grandfathering -- every change must be idempotent\".
--
-- If the tag is 'Just' but does not appear in the plan, 'fail' (a typo
-- in the tag should not silently pass as \"nothing is grandfathered\").
--
-- Reworked changes (a line of the form @name [name\@tag]@) become two
-- steps: the predecessor step deploys through @\@<tag>@ and reads
-- @deploy/name\@<tag>.sql@; the head step deploys via @name\@HEAD@ and
-- reads @deploy/name.sql@.
readSqitchPlan ::
  -- | Grandfather tag (without the leading @\@@), or 'Nothing'.
  Maybe Text ->
  Path Abs File ->
  IO [PlanStep]
readSqitchPlan mGrandfatherTag path = do
  contents <- Text.decodeUtf8Lenient <$> SB.readFile (fromAbsFile path)
  let allLines = Text.lines contents
      -- When there is no grandfather tag, no change is grandfathered;
      -- this is modelled by starting "past the tag" immediately.
      pastTagInitial = case mGrandfatherTag of
        Nothing -> True
        Just _ -> False
      steps = go pastTagInitial Map.empty allLines
      tagSeen = case mGrandfatherTag of
        Nothing -> True
        Just t -> any (tagLineMatches t) allLines
  if not tagSeen
    then
      fail $
        "sydtest-sqitch: grandfather tag '"
          <> Text.unpack (fromMaybe "" mGrandfatherTag)
          <> "' was not found in "
          <> fromAbsFile path
    else pure steps
  where
    -- A line is a tag declaration line if (after stripping) it starts with
    -- '@'. Its name is the run of non-space characters after the '@'.
    tagLineMatches :: Text -> Text -> Bool
    tagLineMatches t line =
      let stripped = Text.strip line
       in case Text.uncons stripped of
            Just ('@', rest) ->
              Text.takeWhile (\c -> c /= ' ' && c /= '\t') rest == t
            _ -> False

    -- pastTag is False until we cross the grandfather tag (or always True
    -- if there is no tag). Steps deployed while pastTag is False are
    -- grandfathered.
    go _ _ [] = []
    go pastTag seen (line : rest) =
      let stripped = Text.strip line
       in case Text.uncons stripped of
            Nothing -> go pastTag seen rest
            Just ('%', _) -> go pastTag seen rest
            Just ('@', tagRest) ->
              let tagName = Text.takeWhile (\c -> c /= ' ' && c /= '\t') tagRest
                  pastTag' = pastTag || Just tagName == mGrandfatherTag
               in go pastTag' seen rest
            _ -> case Text.words stripped of
              (name : _) ->
                let grandfathered = not pastTag
                    isHead = Map.member name seen
                    step = case findNextTagBeforeName name rest of
                      Just tagName ->
                        PlanStep
                          { stepLabel = name <> "@" <> tagName,
                            stepDeployTarget = "@" <> tagName,
                            stepScriptName = name <> "@" <> tagName,
                            stepIsGrandfathered = grandfathered,
                            stepIsReworkHead = False
                          }
                      Nothing ->
                        PlanStep
                          { stepLabel = name,
                            stepDeployTarget =
                              if isHead then name <> "@HEAD" else name,
                            stepScriptName = name,
                            stepIsGrandfathered = grandfathered,
                            stepIsReworkHead = isHead
                          }
                 in step : go pastTag (Map.insertWith (+) name (1 :: Int) seen) rest
              [] -> go pastTag seen rest

    -- For a change line @name@ at position p, look forward for the next
    -- tag line that comes before any subsequent line that also begins
    -- with @name@. If found, this is a reworked change and we want the
    -- predecessor (deploy through the tag) target.
    findNextTagBeforeName :: Text -> [Text] -> Maybe Text
    findNextTagBeforeName = scan Nothing
      where
        scan _ _ [] = Nothing
        scan tag nm (l : ls) =
          let stripped = Text.strip l
           in case Text.uncons stripped of
                Nothing -> scan tag nm ls
                Just ('%', _) -> scan tag nm ls
                Just ('@', tagRest) ->
                  let tagName = Text.takeWhile (\c -> c /= ' ' && c /= '\t') tagRest
                   in scan (Just tagName) nm ls
                _ -> case Text.words stripped of
                  (n : _) | n == nm -> tag
                  _ -> scan tag nm ls
