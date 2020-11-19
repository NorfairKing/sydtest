{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Control.Exception
import System.Exit
import Test.Syd

data DangerousRecord = Cons1 {field :: String} | Cons2

main :: IO ()
main = do
  results <- runSpecForest test
  printOutputSpecForest results

test :: SpecForest Test
test =
  [ SpecifyNode "Passes" (pure ()),
    DescribeNode
      "error"
      [ SpecifyNode "Pure error" (pure (error "foobar")),
        SpecifyNode "Impure error" (error "foobar")
      ],
    DescribeNode
      "undefined"
      [ SpecifyNode "Pure undefined" (pure undefined),
        SpecifyNode "Impure undefined" (undefined)
      ],
    SpecifyNode "Exit code" (exitWith $ ExitFailure 1),
    DescribeNode
      "Pure exceptions"
      [ SpecifyNode "Record construction error" (throw $ RecConError "test"),
        SpecifyNode "Record construction error" (pure (seq (let c = Cons1 {} in field c) ())),
        SpecifyNode "Record construction error" (seq (let c = Cons1 {} in field c) (pure ())),
        SpecifyNode "Record selection error" (throw $ RecSelError "test"),
        SpecifyNode "Record selection error" (pure (seq (let c = Cons2 in field c) ())),
        SpecifyNode "Record selection error" (seq (let c = Cons2 in field c) (pure ())),
        SpecifyNode "Record update error" (throw $ RecUpdError "test"),
        SpecifyNode "Record update error" (pure (seq (let c = Cons2 in c {field = "this will throw"}) ())),
        SpecifyNode "Record update error" (seq (let c = Cons2 in c {field = "this will throw"}) (pure ())),
        SpecifyNode "Pattern matching error" (throw $ PatternMatchFail "test"),
        SpecifyNode "Pattern matching error" (pure (seq (let Cons1 s = Cons2 in s) ())),
        SpecifyNode "Pattern matching error" (seq (let Cons1 s = Cons2 in s) (pure ()))
      ],
    DescribeNode
      "Printing"
      [ SpecifyNode "print" (print "hi"),
        SpecifyNode "putStrLn" (putStrLn "hi")
      ]
  ]
