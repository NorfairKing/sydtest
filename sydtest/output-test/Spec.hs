{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-missing-methods -fno-warn-partial-fields -fno-warn-incomplete-uni-patterns -fno-warn-incomplete-record-updates #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.List
import Data.Text (Text)
import Rainbow
import System.Exit
import Test.QuickCheck
import Test.Syd
import Test.Syd.OptParse

data DangerousRecord = Cons1 {field :: String} | Cons2

class ToUnit a where
  toUnit :: a -> ()

instance ToUnit Int -- No implementation on purpose

main :: IO ()
main = do
  sets <- getSettings
  testForest <- execTestDefM sets spec
  _ <- runSpecForestInterleavedWithOutputSynchronously (settingColour sets) (settingFailFast sets) testForest
  _ <- runSpecForestInterleavedWithOutputAsynchronously (settingColour sets) (settingFailFast sets) 8 testForest
  rf1 <- timeItT $ runSpecForestSynchronously (settingFailFast sets) testForest
  printOutputSpecForest (settingColour sets) rf1
  rf2 <- timeItT $ runSpecForestAsynchronously (settingFailFast sets) 8 testForest
  printOutputSpecForest (settingColour sets) rf2
  pure ()

spec :: Spec
spec = do
  it "Passes" (pure () :: IO ())
  describe "error" $ do
    it "Pure error" (pure (error "foobar") :: IO ())
    it "Impure error" (error "foobar" :: IO ())
  describe "undefined" $ do
    it "Pure undefined" (pure undefined :: IO ())
    it "Impure undefined" (undefined :: IO ())
  it "Exit code" $ do
    exitWith $ ExitFailure 1 :: IO ()
  describe "exceptions" $ do
    it "Record construction error" (throw $ RecConError "test" :: IO ())
    exceptionTest "Record construction error" $ let c = Cons1 {} in field c
    it "Record selection error" (throw $ RecSelError "test" :: IO ())
    exceptionTest "Record selection error" $ let c = Cons2 in field c
    it "Record update error" (throw $ RecUpdError "test" :: IO ())
    exceptionTest "Record update error" $ let c = Cons2 in c {field = "this will throw"}
    it "Pattern matching error" (throw $ PatternMatchFail "test" :: IO ())
    exceptionTest "Pattern matching error" $ let Cons1 s = Cons2 in s
    it "ArithException" (throw Underflow :: IO ())
    exceptionTest "Pattern matching error" $ 1 `div` (0 :: Int)
    it "NoMethodError" (throw (NoMethodError "test") :: IO ())
    exceptionTest "Pattern matching error" $ toUnit (5 :: Int)
  describe "Printing" $ do
    it "print" $ print 'a'
    it "putStrLn" $ putStrLn "hi"
  modifyMaxSuccess (`div` 10) $
    modifyMaxSize (`div` 1) $
      modifyMaxShrinks (const 1) $
        modifyMaxDiscardRatio (const 1) $
          describe "Property tests" $ do
            describe "pure" $ do
              it "reversing a list twice is the same as reversing it once" $
                property $
                  \ls -> reverse (reverse ls) == (ls :: [Int])
              it "should fail to show that sorting does nothing" $
                property $
                  \ls -> sort ls == (ls :: [Int])
              it "should work with custom generators too" $ forAll arbitrary $ \b -> b || True
            describe "impure" $ do
              it "reversing a list twice is the same as reversing it once" $
                property $
                  \ls -> reverse (reverse ls) `shouldBe` (ls :: [Int])
              it "should fail to show that sorting does nothing" $
                property $
                  \ls -> sort ls `shouldBe` (ls :: [Int])
              it "should work with custom generators too" $ forAll arbitrary $ \b -> (b || True) `shouldBe` True
  describe "Long running tests" $
    forM_ [1 :: Int .. 10] $
      \i ->
        it (concat ["takes a while (", show i, ")"]) $
          threadDelay 100_000
  describe "Diff" $ do
    it "shows nice multi-line diffs" $
      ("foo", replicate 7 "quux", "bar") `shouldBe` (("foofoo", replicate 6 "quux", "baz") :: (String, [String], String))
    it "shows nice multi-line diffs" $
      ("foo", [], "bar") `shouldBe` (("foofoo", replicate 6 "quux", "baz") :: (String, [String], String))
  describe "assertions" $ do
    it "shouldBe" $ 3 `shouldBe` (4 :: Int)
    it "shouldNotBe" $ 3 `shouldNotBe` (3 :: Int)
    it "shouldSatisfy" $ (3 :: Int) `shouldSatisfy` even
    it "shouldNotSatisfy" $ (3 :: Int) `shouldNotSatisfy` odd
  pending "pending test"
  describe "Golden" $ do
    it "does not fail the suite when an exception happens while reading" $
      GoldenTest
        { goldenTestRead = die "test",
          goldenTestProduce = pure (),
          goldenTestWrite = \() -> do
            pure (),
          goldenTestCompare = \() () -> Nothing
        }
    it "does not fail the suite when an exception happens while producing" $
      GoldenTest
        { goldenTestRead = pure Nothing,
          goldenTestProduce = die "test",
          goldenTestWrite = \() -> do
            pure (),
          goldenTestCompare = \() () -> Nothing
        }
    it "does not fail the suite when an exception happens while writing" $
      GoldenTest
        { goldenTestRead = pure Nothing,
          goldenTestProduce = pure (),
          goldenTestWrite = \() -> die "test",
          goldenTestCompare = \() () -> Nothing
        }
    it "does not fail the suite when an exception happens while checking for equality" $
      GoldenTest
        { goldenTestRead = pure (Just ()),
          goldenTestProduce = pure (),
          goldenTestWrite = \() -> pure (),
          goldenTestCompare = \actual expected -> case 1 `div` (0 :: Int) of
            1 -> Nothing
            _ ->
              if actual == expected
                then Nothing
                else Just $ NotEqualButShouldHaveBeenEqual (show actual) (show expected) Nothing
        }

    describe "outputResultForest" $ do
      it "outputs the same as last time" $ do
        pureGoldenByteStringFile
          "test_resources/output.golden"
          (SB8.intercalate (SB8.pack "\n") $ map SB.concat $ outputSpecForestByteString toByteStringsColors256 (Timed [] 0))
  doNotRandomiseExecutionOrder $
    describe "Around" $
      do
        describe "before" $ do
          before (() <$ throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "before_" $ do
          before_ (throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "after" $ do
          after (\_ -> throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "after_" $ do
          after_ (throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "around" $ do
          around (\_ -> throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "around_" $ do
          around_ (\_ -> throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "aroundWith" $ do
          aroundWith (\_ () -> throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()

        describe "aroundWith'" $ do
          aroundWith' (\_ () () -> throwIO (userError "test")) $
            it "does not kill the test suite" $ \() ->
              pure () :: IO ()
  it "expectationFailure" (expectationFailure "fails" :: IO ())
  describe "String" $ do
    it "compares strings" $ ("foo\nbar\tquux " :: String) `shouldBe` "foq\nbaz\tqex"
    it "compares strings" $ ("foo\nbar\tquux " :: String) `stringShouldBe` "foq\nbaz\tqex"
    it "compares texts" $ ("foo\nbar\tquux " :: Text) `shouldBe` "foq\nbaz\tqex"
    it "compares texts" $ ("foo\nbar\tquux " :: Text) `textShouldBe` "foq\nbaz\tqex"
    it "compares bytestrings" $ ("foo\nbar\tquux " :: ByteString) `shouldBe` "foq\nbaz\tqex"
  describe "Property" $ do
    it "shows many generated values too" $
      property $ \i ->
        property $ \j ->
          property $ \k ->
            property $ \l ->
              property $ \m ->
                i + j + k + l + m `shouldBe` m + l + k + j + i + (1 :: Int)
    it "shows the classes in use on success" $
      forAll (sort <$> arbitrary) $ \xs ->
        classify (length xs > 1) "non-trivial" $
          sort xs `shouldBe` (xs :: [Int])
    it "shows the classes in use on failure" $
      forAll (sort <$> arbitrary) $ \xs ->
        classify (length xs > 1) "non-trivial" $
          sort xs `shouldBe` (0 : xs :: [Int])

exceptionTest :: String -> a -> Spec
exceptionTest s a = describe s $ do
  it "fails in IO, as the result" (pure (seq a ()) :: IO ())
  it "fails in IO, as the action" (seq a (pure ()) :: IO ())
  it "fails in pure code" $ seq a True
