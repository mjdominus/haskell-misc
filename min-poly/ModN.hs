module ModN (mod2,mod3,mod4,mod5,mod6,mod7) where

data Zero
data Succ a

lower :: Succ a -> a
lower = undefined

class Modulus a where
  modulus :: Num b => a -> b

instance Modulus Zero where
  modulus = \a -> 0

instance Modulus a => Modulus (Succ a) where
  modulus = \a -> 1 + modulus (lower a)

m2 = undefined :: Succ (Succ Zero)
m3 = undefined :: Succ (Succ (Succ Zero))
m4 = undefined :: Succ (Succ (Succ (Succ Zero)))
m5 = undefined :: Succ (Succ (Succ (Succ (Succ Zero))))
m6 = undefined :: Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
m7 = undefined :: Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

data ModN m n = ModN (m,n)

mod2 n = ModN  (m2, n)
mod3 n = ModN  (m3, n)
mod4 n = ModN  (m4, n)
mod5 n = ModN  (m5, n)
mod6 n = ModN  (m6, n)
mod7 n = ModN  (m7, n)

instance (Modulus m, Integral n) => Show (ModN m n) where
  show (ModN (m, n)) = show (n `mod` modulus m) ++
                       " (mod " ++ show (modulus m) ++ ")"

instance (Modulus m, Integral n) => Eq (ModN m n)
  where ModN(m, n) == ModN(_, n') = ((n - n') `mod` modulus m) == 0

instance (Modulus m, Integral n) => Num (ModN m n)
  where ModN(m, n) + ModN(_, n') = ModN(m, (n + n') `mod` modulus m)
        ModN(m, n) - ModN(_, n') = ModN(m, (n - n') `mod` modulus m)
        ModN(m, n) * ModN(_, n') = ModN(m, (n * n') `mod` modulus m)
        negate (ModN(m,n)) = ModN(m,0) - ModN(m, n)
        fromInteger _ = undefined
        signum _  = undefined
        abs n = n


