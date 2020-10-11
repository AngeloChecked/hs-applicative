module CustomListSpec where

import CustomList 
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do 
        a <- arbitrary
        frequency [ (1, return (Cons a (Cons a Nil)) )
                  , (1, return Nil) ]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Custom List" $
        it "quickbatch" $ 
            property $ do 
                quickBatch (monoid (Cons "a" Nil))
                quickBatch (applicative (Cons ("ciao",1::Int,'a') Nil))
    describe "hints" $ do 
        it "custom list applicative funtionment" $ 
            (Cons (+1) (Cons (*2) Nil)) <*> Cons 1 (Cons 2 Nil) `shouldBe` Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))) 
        it "flatMap" $
            flatMap  (\x -> x `Cons` (9 `Cons` Nil)) (Cons 1 (Cons 2 (Cons 3 Nil))) `shouldBe` Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))))


