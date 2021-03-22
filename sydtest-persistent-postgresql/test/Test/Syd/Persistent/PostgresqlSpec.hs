module Test.Syd.Persistent.PostgresqlSpec (spec) where

import Database.Persist
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Postgresql

spec :: Spec
spec = persistPostgresqlSpec migrateExample $
  it "can write and read this example person" $ \pool ->
    runPostgresqlTest pool $ do
      let p = Person {personName = "John Doe", personAge = Just 21}
      i <- insert p
      mp <- get i
      liftIO $ mp `shouldBe` Just p
