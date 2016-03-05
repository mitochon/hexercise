import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


bind :: Monad m => (a -> m b) -> m a -> m b
bind f n = join $ fmap f n


data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap f a = NopeDotJpg
  
instance Applicative Nope where
  pure a = NopeDotJpg
  f <*> g = NopeDotJpg
  
instance Monad Nope where
  return a = NopeDotJpg
  f >>= g = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = elements [ NopeDotJpg ]

instance Eq a => EqProp (Nope a) where (=-=) = eq


main :: IO()
main = let trigger = undefined :: Nope (Int, Int, Int)
       in quickBatch $ monad trigger


data PhhhbbtttEither b a = Lefty a | Righty b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Lefty a) = Lefty (f a)
  fmap f (Righty b) = Righty b

instance Applicative (PhhhbbtttEither b) where
  pure a = Lefty a
  _ <*> Righty b = Righty b
  Righty b <*> _ = Righty b
  Lefty f <*> Lefty a = Lefty (f a)

instance Monad (PhhhbbtttEither b) where
  return a = Lefty a
  Lefty a >>= f = f a
  Righty b >>= _ = Righty b
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Lefty a, Righty b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

                                       
main2 :: IO()
main2 = let trigger = undefined :: PhhhbbtttEither Int (Int, Int, Int)
        in quickBatch $ monad trigger


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
  
instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

main3 :: IO()
main3 = let trigger = undefined :: Identity (Int, Int, Int)
        in quickBatch $ monad trigger


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)


instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f g <*> a = append (fmap f a) (g <*> a)

instance Monad List where
  return = pure
  a >>= f = flatMap f a

-- from ch17  
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

main4 :: IO()
main4 = let trigger = undefined :: List (Int, Int, Int)
        in quickBatch $ monad trigger



j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh l f = let g = map f l in sequence g

meh2 :: Monad m => [a] -> (a -> m b) -> m [b]
meh2 [] _ = pure []
meh2 (x:xs) f = do
  y <- f x
  z <- meh2 xs f
  pure (y : z)


meh3 :: Monad m => [a] -> (a -> m b) -> m [b]
meh3 [] _ = pure []
meh3 (x:xs) f = f x >>= \y -> (meh3 xs f) >>= \z -> return (y:z)


flipType :: (Monad m) => [m a] -> m [a]
flipType = sequence


flipType2 :: (Monad m) => [m a] -> m [a]
flipType2 k = meh k id


