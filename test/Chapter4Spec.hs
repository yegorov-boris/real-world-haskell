module Chapter4Spec where

import Test.Hspec
import Chapter4

spec :: Spec
spec = do
  describe "Chapter4 Tests" $ do
    describe "safeHead" $ do
      it "should work for an empty list" $ safeHead "" `shouldBe` Nothing
      it "should work for a not empty list" $ safeHead "a" `shouldBe` Just 'a'
    describe "safeTail" $ do
      it "should work for an empty list" $ safeTail "" `shouldBe` Nothing
      it "should work for a not empty list" $ safeTail "a" `shouldBe` Just ""
    describe "safeLast" $ do
      it "should work for an empty list" $ safeLast "" `shouldBe` Nothing
      it "should work for a not empty list" $ safeLast "a" `shouldBe` Just 'a'
    describe "safeInit" $ do
      it "should work for an empty list" $ safeInit "" `shouldBe` Nothing
      it "should work for a not empty list" $ safeInit "a" `shouldBe` Just ""
    describe "splitWith" $ do
      it "should work for an empty list" $ splitWith (/= ' ') "" `shouldBe` []
      it "should work for not empty lists" $ do
        splitWith (/= ' ') "  " `shouldBe` []
        splitWith (/= ' ') "a  " `shouldBe` ["a"]
        splitWith (/= ' ') " a  " `shouldBe` ["a"]
        splitWith (/= ' ') "ab  " `shouldBe` ["ab"]
        splitWith (/= ' ') "   ab  c d   ef" `shouldBe` ["ab", "c", "d", "ef"]
    describe "asInt_either" $ do
      it "should parse Int from String" $ do
        asInt_either "101" `shouldBe` Right 101
        asInt_either "-31337" `shouldBe` Right (-31337)
        asInt_either "1798" `shouldBe` Right 1798
        asInt_either "" `shouldBe` Right 0
        asInt_either "-" `shouldBe` Right 0
        asInt_either "-3" `shouldBe` Right (-3)
        asInt_either "33" `shouldBe` Right 33
      it "should fail on incorrect string" $ do
        asInt_either "2.7" `shouldBe` Left "non-digit '.'"
        asInt_either "foo" `shouldBe` Left "non-digit 'o'"
