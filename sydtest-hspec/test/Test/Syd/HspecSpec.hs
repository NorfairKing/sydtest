module Test.Syd.HspecSpec (spec) where

import Control.Concurrent.STM
import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Typeable (cast)
import Test.Hspec as Hspec
import Test.Hspec.QuickCheck as Hspec
import qualified Test.Syd as Syd
import qualified Test.Syd.Hspec as Syd
import qualified Test.Syd.OptParse as Syd

-- | An exception that a test throws itself, but that is classified as
-- asynchronous, as @effectful@'s @Error@ effect does.
-- See https://github.com/NorfairKing/sydtest/issues/110
data SynchronousButClassifiedAsynchronous = SynchronousButClassifiedAsynchronous
  deriving (Show, Eq)

instance Exception SynchronousButClassifiedAsynchronous where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Syd.Spec
spec = do
  Syd.it "fails an hspec test that throws an asynchronously-classified exception with that exception" $ do
    let settings = Syd.defaultSettings {Syd.settingRetries = 1}
    specForest <-
      Syd.execTestDefM settings $
        Syd.fromHspec $
          it "throws" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    resultForest <- Syd.runSpecForestSynchronously settings specForest
    map
      (NE.map (\r -> (Syd.testRunResultStatus r, Syd.testRunResultException r >>= fromException >>= \(Syd.Contextual e _) -> (cast e :: Maybe SomeException) >>= fromException)) . Syd.testRunReportRawResults . Syd.timedValue . Syd.testDefVal . snd)
      (Syd.flattenSpecForest (Syd.timedValue resultForest))
      `Syd.shouldBe` [(Syd.TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]
  Syd.fromHspec exampleHspecSpec

exampleHspecSpec :: Hspec.Spec
exampleHspecSpec = do
  it "adds 3 and 5 together purely" $ 3 + 5 == (8 :: Int)
  it "adds 3 and 5 together in io" $ 3 + 5 `shouldBe` (8 :: Int)
  prop "works for a property as well" $ \ls -> reverse (reverse ls) `shouldBe` (ls :: [Int])
  describe "before" $ do
    var <- runIO $ newTVarIO (1 :: Int)
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    before readAndIncrement $ do
      it "reads 2" $ \i ->
        i `shouldBe` 2
      it "reads 3" $ \i ->
        i `shouldBe` 3
      it "reads 4" $ \i ->
        i `shouldBe` 4
