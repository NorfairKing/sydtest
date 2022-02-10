module Main where

import Spec
import Test.Syd
import Test.Syd.OptParse

main :: IO ()
main = do
  _ <- sydTestResult defaultSettings spec
  pure ()
