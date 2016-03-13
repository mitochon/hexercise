{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }


instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName, dogName :: DogName, address :: Address } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName, dogsAddress :: Address } deriving (Eq, Show)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Reader Person Dog
--getDogR' = Reader $ \p -> Dog (dogName p) (address p) -- ooh, bad soln
getDogR' = Dog <$> Reader dogName <*> Reader address


instance Monad (Reader r) where
  return = pure
  
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r


getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader address >>=
            \a -> Reader dogName >>=
                  \d -> Reader $ \p -> Dog d a

