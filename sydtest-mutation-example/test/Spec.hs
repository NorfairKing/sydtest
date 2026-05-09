module Main (main) where

import Example.LibSpec (spec)
import Test.Syd (sydTest)

main :: IO ()
main = sydTest spec
