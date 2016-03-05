module Ch20 where

import Data.Monoid

-- Implement the functions in terms of foldMap or foldr from Foldable,
-- foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . (Prelude.foldMap Sum)

product :: (Foldable t, Num a) => t a -> a
product =  getProduct . (Prelude.foldMap Product)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = foldr check False
  where check = (\n acc -> if (acc || n == e)
                           then True else False)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = comparer LT

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = comparer GT

comparer :: (Foldable t, Ord a) => Ordering -> t a -> Maybe a
comparer ord xs = foldr comp Nothing xs
  where comp = (\n acc -> case acc of
                   Nothing -> Just n
                   Just c -> if (compare n c == ord) then Just n else Just c)

null :: (Foldable t) => t a -> Bool
null = foldr exist True
  where exist = (\_ acc -> False)

length :: (Foldable t) => t a -> Int
length = foldr addup 0
  where addup = (\_ acc -> acc + 1)

toList :: (Foldable t) => t a -> [a]
toList = foldr append []
  where append = (\e acc -> e : acc)

-- Hint: use foldMap.
-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = Prelude.foldMap id

-- Define foldMap in terms of foldr.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr combine mempty
  where combine = (\e acc -> mappend (f e) acc)


-- Write Foldable instances for the following datatypes.
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr f x (Constant a) = x

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f x (Two a b) = f b x
    
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f x (Three a b c) = f c x
    
data Three' a b = Three' a b b deriving (Show)

instance Foldable (Three' a) where
  foldr f x (Three' a b c) = f c x
    
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f x (Four' a b c d) = f d x


-- | Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = Prelude.foldMap g
  where g y = if (f y) then pure y else mempty
