{-# LANGUAGE GADTs #-}

module Relator (Relator(Rel), relmod,
                s2, s3, im,
                Poly(Poly)
               ) where

import Polynomial (Poly(Poly), degree, leading, poly_c)
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
  if degree p < liftrel degree rel then p
  else
    relmod rel p'
    where f = leading p / liftrel leading rel
          p' = p - liftrel (poly_c f) rel

-- square root of n
sqrt n = Rel (Poly [-n, 0/1, 1/1])
s2 = sqrt 2
s3 = sqrt 3
im = sqrt (-1)

