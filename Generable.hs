
class Monad m => Generable m a where
  gen :: m a

instance (Generable m a, Generable m b) => Generable m (a, b) where
  gen = (gen, gen)



  