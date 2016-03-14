module RandomExample where 

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- newtype State s a =
--   State { runState :: s -> (a, s) }

-- Six-sided die
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)
  

intToDie :: Int -> Die 
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x


rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
-- this will produce the same results every
-- time because it is free of effects.
-- This is fine for this demonstration.
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)    


rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


-- keep rolling until we reach or exceed 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen


rollsToGetN :: Int -> StdGen -> Int
rollsToGetN i g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= i = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged i g = go 0 0 g []
  where go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
        go sum count gen acc
          | sum >= i = (count, acc)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen (intToDie die : acc)
