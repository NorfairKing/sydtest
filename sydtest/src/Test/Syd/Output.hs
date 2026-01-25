module Test.Syd.Output
  ( -- * Main dispatch function
    printOutputSpecForest,

    -- * Re-exports
    module Test.Syd.Output.Common,
    module Test.Syd.Output.Pretty,
    module Test.Syd.Output.Terse,
  )
where

import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import Test.Syd.OptParse
import Test.Syd.Output.Common
import Test.Syd.Output.Pretty
import Test.Syd.Output.Terse
import Test.Syd.Run (Timed)
import Test.Syd.SpecDef

printOutputSpecForest :: Settings -> Timed ResultForest -> IO ()
printOutputSpecForest settings results =
  LTIO.putStr $
    LTB.toLazyText $
      let renderer =
            case settingOutputFormat settings of
              OutputFormatTerse -> renderTerseSummary
              OutputFormatPretty -> renderPrettyReport
       in renderer settings results
