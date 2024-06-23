module Main where

import qualified Bar.Spec as Bar
import qualified Baz.Qux.QuxSpec as Qux
import qualified Foo.Spec as Foo
import Test.Syd

main :: IO ()
main = sydTest $ do
  Foo.spec
  beforeAll (pure "bar") Bar.spec
  Qux.spec
