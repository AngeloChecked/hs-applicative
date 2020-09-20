module PrSpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "a" $ it "b" $ 1 `shouldBe` 1
