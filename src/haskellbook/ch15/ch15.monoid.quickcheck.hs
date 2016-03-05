import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

type S = String
type B = Bool

main :: IO()
main = do
  quickCheck (monoidAssoc :: S -> S -> S -> B)
  quickCheck (monoidLeftIdentity :: S -> B)
  quickCheck (monoidRightIdentity :: S -> B)
