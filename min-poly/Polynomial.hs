
module Polynomial (Poly(Poly), degree, leading,
                   poly_c, poly_shift,
                   showPoly) where

data Poly a = Poly [a] deriving (Eq, Show)

instance Functor Poly where
  fmap f (Poly a) = Poly (fmap f a)

-- bad, but good enough for now
showPoly :: (Show a) => [Char] -> Poly a -> [Char]
showPoly var (Poly cs) = showPoly' 0 var cs
  where showPoly' n var (c:cs) = showMono' n var c ++
                                 " + " ++
                                 showPoly' (n+1) var cs
        showPoly' _ _ [] = ""
        showMono' 0 _ c = show c
        showMono' 1 var c = show c ++ var
        showMono' n var c = show c ++ var ++ "^" ++ (show n)

-- Multiply a polynomial by c
poly_c :: Num c => c -> Poly c -> Poly c
poly_c c        = fmap (* c)

-- Multiply a polynomial by x^d
-- poly_shift :: (Num a, Num c) => a -> Poly c -> Poly c
poly_shift :: Num a => Int -> Poly a -> Poly a
poly_shift d (Poly p)  = Poly $ (replicate d 0) ++ p

longZipWith f a [] = a
longZipWith f [] b = b
longZipWith f (a:as) (b:bs) = (f a b) : longZipWith f as bs

poly_add (Poly a) (Poly b) = trim $ Poly $ longZipWith (+) a b
poly_sub a b = poly_add a (poly_c (-1) b)

poly_mul a (Poly []) = Poly []
poly_mul a (Poly (b:bs)) =
  poly_add (poly_c b a) (poly_x (poly_mul a (Poly bs)))
  where poly_x (Poly a) = Poly (0:a)

plift f (Poly p) = Poly $ f p
plist f (Poly p) = f p

instance (Eq a, Num a) => (Num (Poly a)) where
  (+) = poly_add
  (-) = poly_sub
  (*) = poly_mul
  fromInteger = Poly . (: []) . fromInteger
  abs = undefined
  signum = undefined

-- trim off leading zero terms
trim :: (Eq a, Num a) => Poly a -> Poly a
trim (Poly p) = if (last p == 0) then trim $ Poly $ init p else Poly p

degree :: (Eq a, Num a) => Poly a -> Int
degree = plist length . trim
leading :: (Eq a, Num a) => Poly a -> a
leading = plist last . trim
