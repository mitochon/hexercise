{-# LANGUAGE
RankNTypes,
TupleSections
  #-}

-- | from http://artyom.me/lens-over-tea-1

import Control.Applicative
import Data.Tuple (swap)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a,x) = (swap . (,) x) <$> (f a)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x,a) = ((,) x) <$> (f a)

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> (set s) <$> f (get s)
-- hole :: (a -> f b) -> s -> f t
-- set s :: b -> t
-- f (get s ) :: f b

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = \f e -> case e of
  Left s1 -> Left <$> (l1 f s1)
  Right s2 -> Right <$> (l2 f s2)
-- hole :: (a -> f b) -> Either s1 s2 -> f (Either t1 t2)
-- l1 :: (a -> f b) -> s1 -> f t1
-- l2 :: (a -> f b) -> s2 -> f t2
-- Lens s t a b :: (a -> f b) -> s -> f t

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (\a -> (f a, f a)) s

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\a -> (a, f a)) s

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united = lens (\s -> ()) (\s () -> s)
-- hole :: (() -> f ()) -> s -> f s
-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
