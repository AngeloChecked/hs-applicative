module Applicative where

import Control.Applicative
import Data.List (elemIndex)

-- fmap f x = pure f <*> x
-- in the following exercises, you will need to use the given terms to meke the
-- expressions type check: pure, <$>, <*>
--1)
added :: Maybe Integer
added =
    (+) <$> pure 3 <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
--2)
y :: Maybe Integer
y = lookup 2 $ zip [1,2,3] [4,5,6] 

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6] 

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- why? (,) <$> pure [(1,2)] <*> [(3,4)]
-- pure (1+) <*> (1,1)
-- 3)
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y2 :: Maybe Int
y2 = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y2 

--4)
xs = [1,2,3]
ys = [4,5,6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)
summed2 = fmap sum $ (,) <$> x <*> y

-- write Applicative instance for Identity
newtype Identity a = Identity a
    deriving (Eq,Ord,Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a) 

---
    
newtype Constant a b = 
    Constant { getConstant :: a }
    deriving (Eq,Ord,Show)

instance Functor (Constant a) where
    fmap f (Constant a)  = (Constant a)

instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty 
    (<*>) (Constant a) (Constant b) = Constant (a <> b)
    
--
-- Fixer upper
--1)

a = const <$> Just "Hello" <*> pure "World"

--2)

b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3] 

--





