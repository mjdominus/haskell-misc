import Polynomial
import Relator
import Test.HUnit
import qualified System.Timeout

-- irel = Rel (Poly [1/1, 0, 1/1] )
irel = Rel (Poly [1, 0, 1] )

-- Let's calculate a few powers of i+1 and see what we get

-- Polynomial in i in x
data PolyI a = Im (Poly (Poly a))

instance Show a => Show (PolyI a) where
  show (Im p) = showPoly "x" $ fmap (showPoly "i") p
  
powers x = iterate ((relmod irel) . (* x)) (Poly [1])

iPowerTests = TestList $ 
              map TestCase $
              zipWith (assertEqual "power of i+1") 
              (take 5 (powers (Poly [1, 1])))  -- i+1
              [ Poly [1],
                Poly [1,1],
                Poly [0,2],
                Poly [-2,2],
                Poly [-4]
              ]

tests = iPowerTests

