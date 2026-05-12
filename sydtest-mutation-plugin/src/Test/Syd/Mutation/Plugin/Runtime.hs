module Test.Syd.Mutation.Plugin.Runtime (ifMutation, MutationId (..), mutationTextPack) where

import Data.Text (Text, pack)
import Test.Syd.Mutation.Runtime (MutationId (..), ifMutation)

mutationTextPack :: String -> Text
mutationTextPack = pack
