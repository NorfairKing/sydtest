module Main where

import Spec
import Test.Syd
import Test.Syd.OptParse

main :: IO ()
main = do
  settings <- getSettings
  _ <- sydTestResult settings spec
  pure ()
