module Test.Syd.Persistent.SqliteSpec (spec) where

import Database.Persist
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Sqlite

spec :: Spec
spec = persistSqliteSpec migrateExample $
  it "can write and read this example person" $ \pool ->
    runSqliteTest pool $ do
      let p = Person {personName = "John Doe", personAge = Just 21}
      i <- insert p
      mp <- get i
      liftIO $ mp `shouldBe` Just p
