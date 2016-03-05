import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n _ | n <= 0 = Nil
take' n Nil = Nil
take' n (Cons a b) = Cons a (take' (n-1) b)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f g <*> a = append (fmap f a) (g <*> a)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return Nil)
              , (2, return (Cons a b)) ]

instance Eq a => EqProp (List a) where (=-=) = eq





newtype ZipList' a = ZipList' (List a)
                   deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l
  
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

-- takes damn long to figure out
instance Applicative ZipList' where
  pure a = ZipList' (forever a) where forever a = Cons a (forever a)
  ZipList' Nil <*> _  = ZipList' Nil
  _ <*> ZipList' Nil  = ZipList' Nil
  ZipList' (Cons f g) <*> ZipList' (Cons a b) =
    let ZipList' remainder = ZipList' g <*> ZipList' b
    in ZipList' (Cons (f a) remainder)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary
  

main :: IO()
main = let trigger = undefined :: ZipList' (Int, String, Int)
       in quickBatch (applicative trigger)
