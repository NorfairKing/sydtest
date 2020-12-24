module Test.Syd.Def.Env where

import Control.Monad.Reader
import GHC.Stack
import Test.Syd.Def.Specify
import Test.Syd.Def.TestDefM

-- | For defining a part of a test suite in 'ReaderT IO' instead of in 'IO'.
--
-- This way you can write this:
--
-- > spec :: Spec
-- > spec = around withConnectionPool $
-- >   it "can read what it writes" $ \pool ->
-- >     let person = Person { name = "Dave", age = 25 }
-- >     i <- runSqlPool (insert person) pool
-- >     person' <- runSqlPool (get i) pool
-- >     person' `shouldBe` person
--
-- like this instead:
--
-- > spec :: Spec
-- > spec = around withConnectionPool $
-- >   eit "can read what it writes" $ do
-- >     let person = Person { name = "Dave", age = 25 }
-- >     i <- runDB $ insert person
-- >     person' <- runDB $ get i
-- >     liftIO $ person' `shouldBe` person
-- >
-- > runDB :: ReaderT ConnectionPool IO a -> IO a
--
-- Note that you use `eit` with a property test. In that case you would have to write it like this:
--
-- > spec :: Spec
-- > spec = around withConnectionPool $
-- >   it "can read what it writes" $ \pool -> do
-- >     forAllValid $ \person -> withTestEnv pool $ do
-- >       i <- runDB $ insert person
-- >       person' <- runDB $ get i
-- >       liftIO $ person' `shouldBe` person
eit :: HasCallStack => String -> ReaderT env IO () -> TestDef l env
eit s f = it s (\env -> runReaderT f env)

-- | Helper function to run a property test with an 'env'.
--
-- > withTestEnv = flip runReaderT
withTestEnv :: env -> ReaderT env IO a -> IO a
withTestEnv = flip runReaderT
