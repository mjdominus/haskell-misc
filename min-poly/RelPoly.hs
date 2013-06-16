-- A polynomial expression in an algebraic constant, with an
-- associated name for the constant and a relation on the constant

import Relator
import Polynomial

data RelPoly a = RelPoly String (Relator a) (Poly a)

instance (Show a) => (Show (RelPoly a)) where
  show (RelPoly v r p) = showPoly v p

canonical (RelPoly s r p) = RelPoly s r (relmod r p)

instance (Fractional a, Eq a) => (Eq (RelPoly a)) where
  rp1 == rp2 = cp1 == cp2
    where RelPoly _ _ cp1 = canonical rp1
          RelPoly _ _ cp2 = canonical rp2

instance (Fractional a, Eq a, Num a) => (Num (RelPoly a)) where
  (RelPoly v1 r1 p1) +  (RelPoly v2 r2 p2)
     | v1 == v2 && r1 == r2       = canonical $ RelPoly v1 r1 (p1 + p2)
     | True                       = undefined
  (RelPoly v1 r1 p1) -  (RelPoly v2 r2 p2)
     | v1 == v2 && r1 == r2       = canonical $ RelPoly v1 r1 (p1 - p2)
     | True                       = undefined


(RelPoly v1 r1 p1) ***  (RelPoly v2 r2 p2)
     | v1 == v2 && r1 == r2       = canonical $ RelPoly v1 r1 (p1 * p2)
     | v1 == v2                   = RelPoly v1 r1 (scale (RelPoly (v2 ++ "'") r2 p2) p1)
