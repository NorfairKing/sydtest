{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-missing-methods -fno-warn-partial-fields -fno-warn-incomplete-uni-patterns -fno-warn-incomplete-record-updates #-}

module Spec where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import System.Exit
import System.Random (randomRIO)
import Test.QuickCheck
import Test.Syd
import Test.Syd.OptParse
import Text.Colour

data DangerousRecord = Cons1 {field :: String} | Cons2

class ToUnit a where
  toUnit :: a -> ()

instance ToUnit Int -- No implementation on purpose

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
    let exceptionTest :: String -> a -> Spec
        exceptionTest s a = describe s $ do
          it "fails in IO, as the result" (pure (seq a ()) :: IO ())
          it "fails in IO, as the action" (seq a (pure ()) :: IO ())
          it "fails in pure code" $ seq a True
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
    it "shouldSatisfyNamed" $ shouldSatisfyNamed (3 :: Int) "even" even
    it "shouldNotSatisfyNamed" $ shouldNotSatisfyNamed (3 :: Int) "odd" odd
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
                else Just $ NotEqualButShouldHaveBeenEqual (show actual) (show expected)
        }

    describe "outputResultForest" $ do
      it "outputs the same as last time" $ do
        pureGoldenTextFile
          "test_resources/output.golden"
          (LT.toStrict $ TLB.toLazyText $ renderResultReport defaultSettings With24BitColours (Timed {timedValue = [], timedBegin = 0, timedEnd = 0, timedWorker = 0}))

  doNotRandomiseExecutionOrder $
    describe "Around" $ do
      describe "before" $ do
        before (void (throwIO (userError "test"))) $
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

  describe "Context" $ do
    it "shows a nice context" $ context "Context" $ True `shouldBe` False
    it "shows a nice context multiple levels deep" $
      context "Context1" $
        context "Context2" $
          context "Context3" $
            True `shouldBe` False
    it "shows a context when an exception is thrown as well" $
      context "context" (undefined :: IO ())

  modifyMaxSize (`div` 10) $
    describe "Property" $ do
      describe "0 tests run" $ modifyMaxSuccess (const 0) $ it "shows a red '0 tests' when no tests are run" $ property $ \b -> b `shouldBe` False
      describe "generated values" $
        it "shows many generated values too" $
          property $ \i ->
            property $ \j ->
              property $ \k ->
                property $ \l ->
                  property $ \m ->
                    i + j + k + l + m `shouldBe` m + l + k + j + i + (1 :: Int)
      let magnitude :: Int -> Int
          magnitude = max 0 . (ceiling :: Double -> Int) . logBase 10 . fromIntegral
      describe "labels" $ do
        it "shows the labels in use on success" $
          property $ \xs ->
            label ("length of input is " ++ show (length xs)) $
              reverse (reverse xs) `shouldBe` (xs :: [Int])
        it "shows the labels in use on success" $
          property $ \xs ->
            label ("length of input is " ++ show (length xs)) $
              label ("magnitude (digits) of sum of input is " ++ show (magnitude (sum xs))) $
                reverse (reverse xs) `shouldBe` (xs :: [Int])
        it "shows the labels in use on failure" $
          property $ \xs ->
            label ("length of input is " ++ show (length xs)) $
              label ("magnitude (digits) of sum of input is " ++ show (magnitude (sum xs))) $
                reverse (reverse xs) `shouldBe` (0 : xs :: [Int])

      describe "classes" $ do
        it "shows the classes in use on success" $
          forAll (sort <$> arbitrary) $ \xs ->
            classify (length xs > 1) "non-trivial" $
              sort xs `shouldBe` (xs :: [Int])
        it "shows the classes in use on success" $
          forAll (sort <$> arbitrary) $ \xs ->
            classify (null xs) "empty" $
              classify (length xs == 1) "single element" $
                classify (length xs > 1) "non-trivial" $
                  sort xs `shouldBe` (xs :: [Int])
        it "shows the classes in use on failure" $
          forAll (sort <$> arbitrary) $ \xs ->
            classify (null xs) "empty" $
              classify (length xs == 1) "single element" $
                classify (length xs > 1) "non-trivial" $
                  sort xs `shouldBe` (0 : xs :: [Int])

      describe "tables" $ do
        it "shows the tables in use on success" $
          forAll (sort <$> arbitrary) $ \xs ->
            tabulate "List elements" (map show xs) $
              sort xs `shouldBe` (xs :: [Int])

  modifyMaxSize (const 30) $ -- Bigger than the 20 below
    modifyMaxShrinks (const 30) $ -- Definitely not zero
      describe "Shrinking" $ do
        var <- liftIO newEmptyMVar
        let withVar func = do
              putMVar var ()
              r <- func
              takeMVar var
              pure r
        around_ withVar $
          it "can grab the mvar during shrinking" $
            forAllShrink (sized $ \n -> pure n) shrink $ \i -> do
              () <- readMVar var
              i `shouldSatisfy` (< 20)

  describe "Retries" $ do
    withoutRetries $
      it "does not retry if the test is configured withoutRetries" False
    withRetries 5 $
      it "Retries this five times" False

  describe "Flakiness" $ do
    potentiallyFlaky $ do
      it "Allows flakiness on True eventhough there is none (should succeed)" True
      it "Allows flakiness on False eventhough there is none (should fail)" False
    potentiallyFlakyWith "We're on it!" $ do
      var <- liftIO $ newTVarIO (0 :: Int)
      it "allows this intentionally flaky test with the default number of retries" $ do
        atomically $ modifyTVar' var succ
        i <- readTVarIO var
        i `shouldBe` 2
    notFlaky $ do
      var <- liftIO $ newTVarIO (0 :: Int)
      it "Does not allow flakiness if flakiness is not allowed even if retries happen" $ do
        atomically $ modifyTVar' var succ
        i <- readTVarIO var
        i `shouldBe` 2
    flaky 5 $ it "Allows flakiness in this boolean five times (should fail with 5 retries)" False
    flakyWith 4 "We're on it!" $ do
      var <- liftIO $ newTVarIO (0 :: Int)
      it "allows this intentionally flaky test with up to four retries" $ do
        atomically $ modifyTVar' var succ
        i <- readTVarIO var
        i `shouldBe` 2

  describe "xdescribe" $ do
    xdescribe "two pending tests below here" $ do
      it "one" False
      it "two" True
    xdescribe "four pending tests below here" $ do
      it "one" False
      it "two" True
      describe "wat" $ do
        it "three" False
        it "four" True

  describe "callstack" $ do
    it "it" False
    specify "specify" False
    prop "prop" False
    describe "describe" $ do
      it "describe-it" False
      specify "describe-specify" False

  describe "expectations" $ do
    expectFailing $ do
      it "considered passing" False
      expectPassing $ it "considered passing" True
    expectPassing $ do
      it "considered failing" False
      expectFailing $ it "considered failing" True

  describe "combinators" $ do
    let somePropertyCombinator :: Gen Int -> (Int -> Int) -> Property
        somePropertyCombinator gen func =
          forAll gen $ \i ->
            even (func i)
    it "should fail" $ somePropertyCombinator arbitrary (* 3)
    it "should pass" $ somePropertyCombinator arbitrary (* 4)
    it "should not crash (undefined value)" $ somePropertyCombinator arbitrary undefined
    it "should not crash (undefined generator)" $ somePropertyCombinator undefined (* 2)

    let someTestSuiteCombinator i =
          it "should be even" $ even (i :: Int)
    someTestSuiteCombinator 1
    someTestSuiteCombinator 2
    someTestSuiteCombinator undefined

  describe "randomness" $
    it "always outputs the same pseudorandomness" $ do
      i <- randomRIO (1, 100)
      i `shouldBe` (2 :: Int)
