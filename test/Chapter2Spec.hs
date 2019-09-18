module Chapter2Spec where

import Test.Hspec
import Control.Exception (evaluate)
import Chapter2

spec :: Spec
spec = do
  describe "Chapter2 Tests" $ do
    describe "lastButOne" $ do
      it "should not work for an empty list" $ evaluate (lastButOne "") `shouldThrow` anyException
      it "should not work for a one-element list" $ evaluate (lastButOne "a") `shouldThrow` anyException
      it "should work for a two-elements list" $ lastButOne "ab" `shouldBe` 'a'
      it "should work for a longer list" $ lastButOne "abc" `shouldBe` 'b'
