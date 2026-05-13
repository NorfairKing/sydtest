module Example.DoLib (greet) where

import Control.Monad.Writer (Writer, execWriter, tell)

-- | Build a greeting string, optionally with an exclamation mark.
--
-- Mutation sites: RemoveAction removes one of the three tell actions.
greet :: Bool -> String
greet loud = execWriter go
  where
    go :: Writer String ()
    go = do
      tell "Hello"
      if loud then tell "!" else pure ()
      tell "\n"
