{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.Trans


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }
  
instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)
-- or similarly
--  lift ma = StateT $ \s -> ma >>= \a -> return (a, s)
