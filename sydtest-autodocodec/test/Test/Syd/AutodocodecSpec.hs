{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.AutodocodecSpec (spec) where

import Autodocodec
import Test.Syd
import Test.Syd.Autodocodec

spec :: Spec
spec = do
  describe "pureGoldenYamlSchemaFileViaCodec" $
    it "Example has the same schema as before" $
      pureGoldenYamlSchemaFileVia "test_resources/example.txt" exampleCodec

  describe "pureGoldenYamlSchemaFileViaCodec" $
    it "Int has the same schema as before" $
      pureGoldenYamlSchemaFileViaCodec @Int "test_resources/int.txt"

data Example = Example {exampleInt :: Int, exampleBool :: Bool}

exampleCodec :: JSONCodec Example
exampleCodec =
  object "Example" $
    Example
      <$> requiredField "int" "example int" .= exampleInt
      <*> requiredField "bool" "example bool" .= exampleBool
