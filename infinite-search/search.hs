
import Prelude hiding (pred)

data Nat = Z | S Nat deriving (Eq, Show, Ord)
pred (S n) = n
pred Z = undefined

instance Num Nat where
  x + Z = x
  x + S y = S $ x + y
  x * Z = Z
  Z * y = Z
  (S x) * (S y) = S(x + y + (x * y))
  abs = id
  signum Z = 0
  signum (S _) = 1
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n-1))

inf = S inf

existsFrom f fromN = f fromN || existsFrom f  (S fromN)
exists f = existsFrom f Z
forall f = not $ exists (not . f)

-- return a value that satisfies f, except that it might not
-- but if there is one, it will.
satisfies f = if f Z then Z else S (satisfies (f . S))

-- If this returns at all, it returns a correct answer
-- returns Nothing only if no value satisfies f
-- otherwise returns Just x, where x satisfies f
-- Always returns for any f that always returns
maybe_satisfies f = if f x then Just x else Nothing
  where x = satisfies f

-- if any value satisfies f, then (satisfies f) satisfies f
-- so there exists an x for whic f x, if and only if f (satisfies f).
xists f = f (satisfies f) 
-- f holds for all values if and only if there is no value satisfying not.f .
frall f = not $ xists (not . f)

-- surprising example
--   xists (\x -> x*x == 15)
-- says "False", which is correct, and does not run forever
