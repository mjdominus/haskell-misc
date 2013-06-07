
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

m7 = undefined :: Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
m6 = undefined :: Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

data ModN m n = ModN (m,n)

mod7 n = ModN  (m7, n)
mod6 n = ModN  (m6, n)

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


