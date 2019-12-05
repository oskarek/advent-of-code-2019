module Day1Spec ( spec ) where

import           Test.Hspec
import           Day1.Day1

spec :: Spec
spec = do
  describe "f" $
    it "handles some simple input" $ do
      f 1 `shouldBe` (-2)
      f 2 `shouldBe` (-2)
      f 3 `shouldBe` (-1)
      f 10 `shouldBe` 1
      f 100 `shouldBe` 31

  describe "g" $
    it "handles some simple input" $ do
      g 1 `shouldBe` 0
      g 2 `shouldBe` 0
      g 3 `shouldBe` 0
      g 10 `shouldBe` 1
      g 100 `shouldBe` 39