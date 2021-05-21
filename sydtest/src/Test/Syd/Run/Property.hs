{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Run.Property where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.IO ()
import Test.QuickCheck.Property hiding (Result (..))
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck.Random
import Test.Syd.Run.Result
import Test.Syd.Run.Settings
import Text.Printf

makeQuickCheckArgs :: TestRunSettings -> Args
makeQuickCheckArgs TestRunSettings {..} =
  stdArgs
    { replay = Just (mkQCGen testRunSettingSeed, 0),
      chatty = False,
      maxSuccess = testRunSettingMaxSuccess,
      maxDiscardRatio = testRunSettingMaxDiscardRatio,
      maxSize = testRunSettingMaxSize,
      maxShrinks = testRunSettingMaxShrinks
    }

runPropertyTestWithArg ::
  (outerArgs -> innerArg -> Property) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runPropertyTestWithArg p trs wrapper = do
  let qcargs = makeQuickCheckArgs trs
  qcr <- quickCheckWithResult qcargs (aroundProperty wrapper p)
  let testRunResultGoldenCase = Nothing
  let testRunResultNumTests = Just $ fromIntegral $ numTests qcr
  case qcr of
    Success {} -> do
      let testRunResultStatus = TestPassed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultExtraInfo = Nothing
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      pure TestRunResult {..}
    GaveUp {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultExtraInfo = Just $ printf "Gave up, %d discarded tests" (numDiscarded qcr)
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      pure TestRunResult {..}
    Failure {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = do
            se <- theException qcr
            pure $ case fromException se of
              Just a -> Right a
              Nothing -> Left $ displayException se
      let testRunResultNumShrinks = Just $ fromIntegral $ numShrinks qcr
      let testRunResultFailingInputs = failingTestCase qcr
      let testRunResultExtraInfo = Nothing
      let testRunResultLabels = Just $ M.singleton (failingLabels qcr) 1
      let testRunResultClasses = Just $ M.fromSet (const 1) (failingClasses qcr)
      let testRunResultTables = Nothing
      pure TestRunResult {..}
    NoExpectedFailure {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      let testRunResultExtraInfo = Just $ printf "Expected the property to fail but it didn't."
      pure TestRunResult {..}

aroundProperty :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Property) -> Property
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a b -> (unGen . unProperty $ p a b) r n

aroundProp :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a b -> unProp $ p a b)

aroundRose :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a b -> reduceRose (r a b) >>= writeIORef ref
  readIORef ref
