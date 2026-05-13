module Example.DoLib (greet) where

import Control.Monad (when)

-- | Print a greeting, optionally with a trailing newline.
--
-- Mutation sites: RemoveAction removes one of the two BodyStmt actions.
greet :: Bool -> IO ()
greet loud = do
  putStr "Hello"
  when loud (putStr "!")
  putStrLn ""
