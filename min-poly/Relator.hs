{-# LANGUAGE GADTs #-}

module Relator (Relator(Rel), relmod,
                s2, s3, im,
                Poly(Poly)
               ) where

import Polynomial (Poly(Poly), degree, leading, poly_c, poly_shift)
import Prelude hiding (sqrt)

-- ----------------------------------------------------------------
-- next task: polynomials under some relation
-- for example, with relator [x^2 - 2] the polynomial x^2 should be
-- reduced to 2.

data Relator p where {
  Rel :: Poly p -> Relator p
} deriving (Show)

liftrel f (Rel p) = f p
-- evaluate a polynomial in some constant x for which we know a relator
-- or, reduce poloynomial p, mod rel
relmod rel p =
  if degree p < degree r then p
  else
    relmod rel p'
  where Rel r = rel
        n = degree p - degree r
        f = leading p / leading r
        p' = p - (poly_shift n $ poly_c f r)

-- square root of n
sqrt n = Rel (Poly [-n, 0/1, 1/1])
s2 = sqrt 2
s3 = sqrt 3
im = sqrt (-1)

