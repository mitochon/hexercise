
newtype State s a =   State { runState :: s -> (a, s) }

-- the state is also the value you return
get :: State s s
get = State $ \s -> (s,s)

-- the state is the argument provided and the value is defaulted to unit
put :: s -> State s ()
put s = State $ \t -> ((), s)


exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \t -> ((), f t)
