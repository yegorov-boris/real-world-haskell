import Test.Hspec
import Chapter3

main :: IO ()
main =  hspec $ do
  describe "Chapter3 Tests" $ do
    describe "len" $ do
      it "should work for an empty list" $ len [] `shouldBe` length []
      it "should work for a not empty list" $ do
        let xs = [42] in len xs `shouldBe` length xs
        let xs = ["a", "b", "c"] in len xs `shouldBe` length xs
    describe "mean" $ do
      it "should work for an empty list" $ mean [] `shouldBe` 0
      it "should work for a not empty list" $ do
        mean [42.73] `shouldBe` 42.73
        mean [42.73, 73.42] `shouldBe` 58.075
    describe "pal" $ do
      it "should work for an empty list" $ let xs = []::[Int] in pal xs `shouldBe` xs
      it "should work for one element lists" $ let xs = [42]::[Int] in pal xs `shouldBe` xs
      it "should work for not empty lists" $
        let
          xs = [42, 73]::[Int]
          p = [42, 73, 73, 42]::[Int]
        in pal xs `shouldBe` p
    describe "isPal" $ do
      it "should work for an empty list" $ isPal ([]::[Int]) `shouldBe` True
      it "should work for not empty lists" $ do
        isPal [42] `shouldBe` True
        isPal [42, 73] `shouldBe` False
        isPal [42, 42] `shouldBe` True
        isPal [42, 42, 42] `shouldBe` True
        isPal [42, 73, 42] `shouldBe` True
        isPal [42, 73, 41] `shouldBe` False
    describe "sortByLen" $ do
      it "should sort by length of elements" $ do
        sortByLen ["a", "", ""] `shouldBe` ["", "", "a"]
    describe "myIntersperse" $ do
      it "should flatten with a separator" $ do
        myIntersperse ',' [] `shouldBe` []
        myIntersperse ',' ["foo"] `shouldBe` "foo"
        myIntersperse ',' ["foo","bar","baz","quux"] `shouldBe` "foo,bar,baz,quux"
