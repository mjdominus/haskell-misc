{-# LANGUAGE GADTs #-}

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

plift f (Poly p) = Poly $ f p
plist f (Poly p) = f p

instance (Num a) => (Num (Poly a)) where
  (+) = poly_add
  (-) = poly_sub
  (*) = poly_mul
  fromInteger = Poly . (: []) . fromInteger
  abs = undefined
  signum = undefined
  
trim (Poly p) = if (last p == 0) then trim $ Poly $ init p else Poly p
degree :: Num a => Poly a -> Int
degree = plist length . trim
leading :: Num a => Poly a -> a
leading = plist last . trim

-- ----------------------------------------------------------------  
-- next task: polynomials under some relation
-- for example, with relator [x^2 - 2] the polynomial x^2 should be
-- reduced to 2.
  
data Relator p where
  Rel :: Poly p -> Relator p

liftrel f (Rel p) = f p
-- evaluate a polynomial in some constant x for which we know a relator
-- or, reduce poloynomial p, mod rel
relmod rel p = 
  if degree p < liftrel degree rel then p
  else
    relmod rel p'
    where f = leading p / liftrel leading rel
          p' = trim $ p - liftrel (poly_c f) rel
