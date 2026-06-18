module Example.UnicodeSpec (spec) where

import Test.Syd

-- Regression: the coverage phase enumerates every leaf test id (via the
-- suite's --mutation-coverage-list child mode), which must emit UTF-8 bytes so
-- that a test described with non-ASCII characters does not crash the child in a
-- C/POSIX-locale build sandbox.
spec :: Spec
spec =
  it "handles a non-ASCII description (café \9749 \10003 \1087\1088\1080\1082\1083\1072\1076)" $
    (1 + 1 :: Int) `shouldBe` 2
