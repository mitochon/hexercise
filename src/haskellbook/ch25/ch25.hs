{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) } deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)
  
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap t (Compose fg) = foldMap (\g -> foldMap t g) fg


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative r => (a -> r b) -> Compose f g a -> r (Compose f g b)
  traverse t (Compose fg) = Compose <$> traverse (\g -> traverse t g) fg

