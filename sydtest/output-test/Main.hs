{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-missing-methods -fno-warn-partial-fields -fno-warn-incomplete-uni-patterns -fno-warn-incomplete-record-updates #-}

module Main where

import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import Spec (spec)
import Test.Syd
import Test.Syd.OptParse
import Text.Colour

main :: IO ()
main = do
  settings <- getSettings
  testForest <- execTestDefM settings spec

  putStrLn "Synchronous, non-interleaved"
  rf1 <- timeItT $ runSpecForestSynchronously settings testForest
  printOutputSpecForest settings rf1

  putStrLn "Synchronous, interleaved"
  _ <- runSpecForestInterleavedWithOutputSynchronously settings testForest

  putStrLn "Asynchronous, non-interleaved"
  rf2 <- timeItT $ runSpecForestAsynchronously settings 8 testForest
  printOutputSpecForest settings rf2

  putStrLn "Asynchronous, interleaved"
  _ <- runSpecForestInterleavedWithOutputAsynchronously settings 8 testForest

  putStrLn "Golden test of output"
  sydTest $
    describe "Golden Output" $
      it "renders output in the same way as before" $
        goldenByteStringFile "test_resources/output-test.txt" $ do
          testForestInOrder <- execTestDefM settings $ doNotRandomiseExecutionOrder spec
          rf <- timeItT $ runSpecForestSynchronously settings testForestInOrder
          let eraseTimed :: Timed a -> Timed a
              eraseTimed t =
                t
                  { timedTime =
                      -- We have to choose zero because it's the identity for addition,
                      -- which is the operation that's used on these times.
                      0
                  }

              erasedTimedInResultForest :: ResultForest -> ResultForest
              erasedTimedInResultForest = fmap (fmap (fmap eraseTimed))
              eraseTiming :: Timed ResultForest -> Timed ResultForest
              eraseTiming = fmap erasedTimedInResultForest . eraseTimed
          pure $
            TE.encodeUtf8
              . LT.toStrict
              . TLB.toLazyText
              . renderResultReport defaultSettings With24BitColours
              $ eraseTiming rf
