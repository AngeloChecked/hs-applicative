module BadMonoidSpec where

import BadMonoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary Bull where
    arbitrary = 
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance EqProp Bull where
    (=-=) = eq

-- applicative :: ( Show a, Show (m a), Show (m (a -> b))
--             , Show (m (b -> c)), Applicative m
--             , CoArbitrary a, EqProp (m a)
--             , EqProp (m b), EqProp (m c)
--             , Arbitrary a, Arbitrary b
--             , Arbitrary (m a)
--             , Arbitrary (m (a->b))
--             , Arbitrary (m (b->c)))
--             => m (a,b,c) -> TestBatch
xs = [("b","w",1::Int)]
type SSI = (String, String, Int)
trigger :: [SSI]
trigger = undefined


spec :: Spec
spec = 
    describe "BadMonoid" $
        it "quickbatch" $ 
            property $ do 
                quickBatch (monoid Twoo)
                --quickBatch (applicative xs)
                --quickBatch (applicative trigger)
            


