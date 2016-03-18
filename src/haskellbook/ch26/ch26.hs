{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema


instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure (pure a)

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT f <*> EitherT a = EitherT $ (<*>) <$> f <*> a


instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT ma >>= f =  EitherT $ do
    v <- ma
    case v of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)


swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mb) = do
  v <- mb
  case v of
    Left a -> f a
    Right b -> g b

-- alternate w/o do block
eitherT' :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT' f g (EitherT mb) = mb >>= (\v -> case v of
                                       Left a -> f a
                                       Right b -> g b
                                   )
               
