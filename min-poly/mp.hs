
data Poly a = Poly [a] deriving (Show, Eq)

instance Functor Poly where
  fmap f (Poly a) = Poly (fmap f a)

poly_c c        = fmap (* c)
                          
longZipWith f a [] = a
longZipWith f [] b = b
longZipWith f (a:as) (b:bs) = (f a b) : longZipWith f as bs

poly_add (Poly a) (Poly b) = Poly $ longZipWith (+) a b
poly_sub a b = poly_add a (poly_c (-1) b)

poly_mul a (Poly []) = Poly []
poly_mul a (Poly (b:bs)) = 
  poly_add (poly_c b a) (poly_x (poly_mul a (Poly bs)))
  where poly_x (Poly a) = Poly (0:a)

instance (Num a) => (Num (Poly a)) where
  (+) = poly_add
  (-) = poly_sub
  (*) = poly_mul
  fromInteger = Poly . (: []) . fromInteger
  abs = undefined
  signum = undefined
  
  

  
