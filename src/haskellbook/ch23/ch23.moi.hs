{-# LANGUAGE InstanceSigs #-}

-- Moi == State, renamed to avoid naming conflict
newtype Moi s a = Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \m -> let (x, n) = g m in (f x, n)

  
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (x, t) = g s
        (y, u) = f t
    in (y x, u)


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) ->Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (x, t) = f s
    in runMoi (g x) t
