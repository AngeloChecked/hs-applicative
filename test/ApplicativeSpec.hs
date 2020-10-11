module ApplicativeSpec where

import Test.Hspec
import Applicative
import Control.Applicative

spec :: Spec
spec = 
    describe "Applicative" $ do
        it "'fmap f x = pure f <*> x' demonstation" $ do
            fmap (+1) [1,2,3] `shouldBe` [2,3,4] 
            pure (+1) <*> [1..3] `shouldBe` [2,3,4]
        it "identity law" $ do
            id [1..5] `shouldBe` fmap id [1..5]
            fmap id [1..5] `shouldBe` (pure id <*> [1..5])
        it "composition" $     
            pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3] `shouldBe` [(+1)] <*> ([(*2)] <*> [1,2,3])
        it "homomorphism" $ do
            (pure (+1) <*> pure 1 :: Either Int Int) `shouldBe` Right 2
            (pure (+1) <*> pure 1 :: Maybe Int) `shouldBe` Just 2 
        it "interchange 'u <*> pure y = pure ($ y) <*> u'" $ do
            (pure ($2) <*> Just (+2)) `shouldBe` Just (+2) <*> pure 2 
            ([(+1),(*2)] <*> pure 1) `shouldBe` (pure ($1) <*> [(+1),(*2)])


