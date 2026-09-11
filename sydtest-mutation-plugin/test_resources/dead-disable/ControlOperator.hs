-- | A disable that names the control (no-op) mutation among its operators.
--
-- 'Control' is a name you see in every mutation report, so reaching for it in
-- an annotation is a natural mistake -- but a control is only inserted where
-- an operator fires, so it is never in the list a disable filters.
module ControlOperator (addOne) where

{-# ANN addOne ("DisableMutations: Arith, Control" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
