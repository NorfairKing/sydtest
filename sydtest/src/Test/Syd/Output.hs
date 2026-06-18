module Test.Syd.Output
  ( -- * Main dispatch function
    printOutputSpecForest,

    -- * Re-exports
    module Test.Syd.Output.Common,
    module Test.Syd.Output.Pretty,
    module Test.Syd.Output.Terse,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LTE
import Test.Syd.OptParse
import Test.Syd.Output.Common
import Test.Syd.Output.Pretty
import Test.Syd.Output.Terse
import Test.Syd.Run (Timed)
import Test.Syd.SpecDef

printOutputSpecForest :: Settings -> Timed ResultForest -> IO ()
printOutputSpecForest settings results =
  -- Encode to UTF-8 and write the bytes, rather than a Text-based putStr that
  -- encodes through the handle's locale encoding: under a C/POSIX locale
  -- (e.g. a Nix build sandbox) that handle is ASCII and crashes on the report's
  -- non-ASCII status markers and box-drawing characters.
  LBS.putStr $
    LTE.encodeUtf8 $
      LTB.toLazyText $
        let renderer =
              case settingOutputFormat settings of
                OutputFormatTerse -> renderTerseSummary
                OutputFormatPretty -> renderPrettyReport
         in renderer settings results
