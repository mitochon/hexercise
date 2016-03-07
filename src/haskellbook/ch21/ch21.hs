{-# LANGUAGE FlexibleContexts #-}

module Ch21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data S n a = S (n a) a deriving (Show, Eq)


instance Functor n => Functor (S n) where
  fmap f (S m a) = S (fmap f m) (f a)

  
instance Applicative n => Applicative (S n) where
  pure a = S (pure a) a
  (S k f) <*> (S m a)  = S (k <*> m) (f a)


instance (Foldable n) => Foldable (S n) where
--  foldMap f (S m a) = (f a) <> (foldMap f m) // THIS IS WRONG
  foldMap f (S m a) = (foldMap f m) <> (f a)


instance Traversable n => Traversable (S n) where
  traverse f (S m a) = S <$> (traverse f m) <*> (f a)


instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq


instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary


type TI = S []

main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (functor trigger)
--  quickBatch (applicative trigger) -- takes forever but passes
  quickBatch (traversable trigger)

