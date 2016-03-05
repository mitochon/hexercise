

(.) :: (b -> c) -> (a -> b) -> a -> c
--      fmap        fmap
fmap::Functor f => (m -> n) -> f m -> f n
fmap::Functor g => (x -> y) -> g x -> g y


-- if   (m -> n) -> (f m -> f n) 
--       a       ->  b
-- then
--      (f m -> f n) -> g (f m ) -> g (f n)
--      (x   -> y)   -> g x      -> g y
--       b           -> c
-- finally
--      (m -> n) -> g (f m) -> g (f n)

