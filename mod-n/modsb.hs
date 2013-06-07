
data End
data ZeroBit a
data OneBit a

class StripBit a where
-- ??

lower :: Succ a -> a
lower = undefined

class Modulus a where
  modulus :: a -> Integer

instance Modulus Zero where
  modulus = \a -> 0

instance (Modulus a) => Modulus (Succ a) where
  modulus = \a -> 1 + modulus (lower a)

data ModN m n = ModN (m,n)

instance (Modulus mm, Integral nn) => Eq (ModN mm nn)
  where ModN(m, n) == ModN(m', n') = ((n - n') `mod` fromInteger (modulus m)) == 0
-- instance (Modulus m, Num n) => Num (ModN m n)

