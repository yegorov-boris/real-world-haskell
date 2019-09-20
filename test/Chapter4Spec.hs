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
    describe "myConcat" $ do
      it "should work with empty lists" $ do
        myConcat ([]::[String]) `shouldBe` ""
        myConcat [""] `shouldBe` ""
        myConcat ["", ""] `shouldBe` ""
      it "should work with not empty lists" $ myConcat ["", "ab", "c", ""] `shouldBe` "abc"
    describe "takeWhileFoldr" $ do
      it "should be takeWhile" $ do
        takeWhileFoldr (/= ' ') "" `shouldBe` ""
        takeWhileFoldr (/= ' ') "   " `shouldBe` ""
        takeWhileFoldr (/= ' ') "abc " `shouldBe` "abc"
        takeWhileFoldr (/= ' ') "a  " `shouldBe` "a"
        takeWhileFoldr (/= ' ') " a" `shouldBe` ""
        takeWhileFoldr (/= ' ') "  a" `shouldBe` ""
    describe "takeWhileRec" $ do
      it "should be takeWhile" $ do
        takeWhileRec (/= ' ') "" `shouldBe` ""
        takeWhileRec (/= ' ') "   " `shouldBe` ""
        takeWhileRec (/= ' ') "abc " `shouldBe` "abc"
        takeWhileRec (/= ' ') "a  " `shouldBe` "a"
        takeWhileRec (/= ' ') " a" `shouldBe` ""
        takeWhileRec (/= ' ') "  a" `shouldBe` ""
    describe "myGroupBy" $ do
      it "should be groupBy" $ myGroupBy (==) "Mississippi" `shouldBe` ["M","i","ss","i","ss","i","pp","i"]
    describe "myAny" $ do
      it "should be any" $ do
        myAny (== 'a') "" `shouldBe` False
        myAny (== 'a') "a" `shouldBe` True
        myAny (== 'a') "ba" `shouldBe` True
        myAny (== 'a') "bc" `shouldBe` False
    describe "myWords" $ do
      it "should be words" $ myWords "Lorem ipsum\ndolor" `shouldBe` ["Lorem","ipsum","dolor"]
    describe "myUnlines" $ do
      it "should be unlines" $ myUnlines ["Hello", "World", "!"] `shouldBe` "Hello\nWorld\n!\n"
    describe "myCycle" $ do
      it "should be cycle" $ do
        myCycle "" `shouldBe` ""
        (head $ myCycle "a") `shouldBe` 'a'
        (head $ tail $ myCycle "a") `shouldBe` 'a'
        (head $ myCycle "ab") `shouldBe` 'a'
        (head $ tail $ myCycle "ab") `shouldBe` 'b'
        (head $ tail $ tail $ myCycle "ab") `shouldBe` 'a'
