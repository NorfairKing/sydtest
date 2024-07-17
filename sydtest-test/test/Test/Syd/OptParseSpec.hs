{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.OptParseSpec (spec) where

import Data.Text (Text)
import Options.Applicative (execParserPure, getParseResult)
import Test.Syd
import Test.Syd.OptParse (Flags (..), flagsParser, prefs_)

spec :: Spec
spec = do
  describe "filter flags" $ do
    it "single flag" $
      parse_filter_flags ["--filter", "foo"] `shouldBe` Just ["foo"]
    it "two flags" $
      parse_filter_flags ["--filter", "foo", "--filter", "bar"] `shouldBe` Just ["foo", "bar"]
    it "single flag with spaces" $
      parse_filter_flags ["--filter", "foo bar"] `shouldBe` Just ["foo bar"]
    it "two flags, one with spaces" $
      parse_filter_flags ["--filter", "foo", "--filter", "bar biz"] `shouldBe` Just ["foo", "bar biz"]
  where
    parse_filter_flags :: [String] -> Maybe [Text]
    parse_filter_flags cli_args =
      let parser_result = execParserPure prefs_ flagsParser cli_args
          mb_flags = getParseResult parser_result :: Maybe Flags
       in flagFilters <$> mb_flags
