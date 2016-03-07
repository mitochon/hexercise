import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node g a h) = Node (fmap f g) (f a) (fmap f h)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node g a h) = (foldMap f g) <> (f a) <> (foldMap f h)
  
instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node g a h) = Node <$> (traverse f g) <*> (f a) <*> (traverse f h)

-- Hints:
-- 1. For foldMap, think Functor but with some Monoid thrown in.
-- 2. For traverse, think Functor but with some Functor3 thrown in.

instance Eq a => EqProp (Tree a) where (=-=) = eq


instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    t <- Node <$> arbitrary <*> arbitrary <*> arbitrary
    elements [ Empty, Leaf a, t]
    
type TI = Tree

main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

