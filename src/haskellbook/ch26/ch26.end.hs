import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> Identity (a - 1)

-- point free
rDec' :: Num a => Reader a a
rDec' = ReaderT $ return . (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> Identity (show a)

-- point free
rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> putStrLn ("Hi: " ++ (show a))
                               >> pure (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> putStrLn ("Hi: " ++ (show a))
                                >> return (show a, (a + 1))
