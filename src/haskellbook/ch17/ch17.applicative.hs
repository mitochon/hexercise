module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
  Fools
  | Twoo
  deriving (Eq, Show)
             

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools) , (1, return Twoo) ]
  

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools
  

instance EqProp Bull where (=-=) = eq

type S = Bull
type B = Bool

main :: IO ()  
main = quickBatch (monoid Twoo)

-- alternatively
-- from ch15 
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

-- main = do
--  quickCheck (monoidAssoc :: S -> S -> S -> B)
--  quickCheck (monoidLeftIdentity :: S -> B)
--  quickCheck (monoidRightIdentity :: S -> B)


{- tried to write applicative laws ... doesn't seem to work
applicativeIdentity :: (Eq (f a), Applicative f) => f a -> Bool
applicativeIdentity a = (pure id <*> a) == a

--applicativeCompose :: (Eq (f a), Applicative f) => f a -> f a -> f a -> Bool
--applicativeCompose u v w =  (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

--applicativeHomomorphism :: (Eq a) => a -> a -> Bool
--applicativeHomomorphism f v = (pure f <*> pure v) == pure (f v)

--applicativeInterchange :: (Eq b, Applicative f) => f a -> b -> Bool
--applicativeInterchange u y = (u <*> pure y) == (pure ($ y) <*> u)
-}

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)


instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f g <*> a = append (fmap f a) (g <*> a)


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys


fold :: ( a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)


concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return Nil)
              , (2, return (Cons a b)) ]

instance Eq a => EqProp (List a) where (=-=) = eq

