{-# LANGUAGE InstanceSigs #-}

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT ma) = StateT $ \s -> fmap q (ma s)
    where q = \(a, t) -> (f a, t)
  

-- note the Monad constraint on m, not Applicative
instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT f <*> StateT ma = StateT $ \s -> 
    ma s >>= \(a, t) -> f t >>= \(g, u) -> pure (g a, u)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT ma >>= f = StateT $ \s ->
    ma s >>= \(a, t) -> runStateT (f a) s
