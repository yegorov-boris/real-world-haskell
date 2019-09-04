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
