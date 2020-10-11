module CustomList where

data List a = 
    Nil
  | Cons a (List a)
  deriving(Eq,Show)


instance Semigroup (List a) where
    (<>) Nil b = b 
    (<>) a Nil = a    
    (<>) (Cons a as) b = Cons a (as <> b)   

instance Monoid (List a) where
    mempty = Nil 
    mappend = (<>)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b) 

instance Applicative List where
    pure a = Cons a Nil 
    (<*>) Nil _ = Nil 
    (<*>) _ Nil = Nil 
    (<*>) (Cons f fs) xxs@(Cons x xs) = (f <$> xxs) <> (fs <*> xxs)    

-- book hints
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs ) ys =
    Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
            -> List a
            -> List b
flatMap f as = concat' $ fmap f as


