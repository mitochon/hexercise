-- given putChar :: Char -> IO ()
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putStrLn' ""

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putChar '\n'

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >>= \ x -> putChar '\n'

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStr' "\n"


getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
            '\n' -> return xs
            _ -> get (xs ++ [x])

-- below wrong - order reversed
getLine2 = get2 ""

get2 :: String -> IO String
get2 xs
  = do x <- getChar
       case x of
       	    '\n' -> return xs
       	    _ -> get2 (x : xs)


sequence_' :: Monad m => [m a] -> m ()
--sequence_' [] = return ()
--sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

--sequence_' [] = return ()
--sequence_' (m : ms) = m >> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m : ms) = m >>= \ _ -> sequence_' ms

sequence_' ms = foldr (>>) (return ()) ms


sequence' :: Monad m => [m a] -> m [a]
--sequence' [] = return []
--sequence' (m : ms) = m >>= \a -> 
--  do as <- sequence' ms
--     return (a : as)

--sequence' ms = foldr func (return []) ms
--  where
--  	func :: (Monad m) => m a -> m [a] -> m [a]
--  	func m acc = do x <- m
--  	                xs <- acc
--  	                return (x : xs)

sequence' [] = return []
sequence' (m : ms) = do a <- m
                        as <- sequence' ms
                        return (a : as)

-- \x -> putChar x >> return x
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM' f as = sequence' (map f as)

--mapM' f [] = return []
--mapM' f (a : as) = f a >>= \ b -> mapM' f as >>= \bs -> return (b : bs)

--mapM' f [] = return []
--mapM' f (a: as) =
--	do
--		b <- f a
--		bs <- mapM' f as
--		return (b : bs)

mapM' f [] = return []
mapM' f (a : as) = f a >>= \b -> 
  do bs <- mapM' f as
     return (b: bs)

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--foldLeftM f a [] = return a 
--foldLeftM f a (x : xs) =
--	do
--		e <- f a x
--		es <- foldLeftM f e xs
--		return es

foldLeftM _ a [] = return a
foldLeftM f a (x : xs) = f a x >>= \fax -> foldLeftM f fax xs

--Question: output of => foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ b [] = return b
foldRightM f b (x : xs) = foldRightM f b xs >>= \frb -> f x frb

--Question:  foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r

liftM' :: Monad m => (a -> b) -> m a -> m b
--liftM' f m = do x <- m
--                return (f x)

liftM' f m = m >>= \a -> return (f a)

-- These will return the same answer / both are wrong ... e.g.
-- Main> liftM' (+1) [3,4]
-- [4,4,5,5]
-- Main> liftM' (+1) [3,4]

--liftM' f m = m >>= \a -> m >>= \ b -> return (f b)
--liftM' f m = m >>= \a -> m >>= \ b -> return (f b)