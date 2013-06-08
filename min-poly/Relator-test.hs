import Relator
import Test.HUnit
import qualified System.Timeout

irel = Rel (Poly [1/1, 0, 1/1] )

reducesTo rel start end = assertEqual name
                                       (relmod rel thePoly) theResult
  where thePoly = Poly start
        theResult = Poly end
        name = show thePoly ++ " -> " ++ show theResult

iReduceTests =
  TestList [ TestLabel "1" $ TestCase $ reducesTo irel [2,0,1]   [1],
             TestLabel "2" $ TestCase $ reducesTo irel [3,0,1]   [2],
             TestLabel "3" $ TestCase $ reducesTo irel [0,0,1]  [-1],
             TestLabel "4" $ TestCase $ reducesTo irel [3,0,2]   [1],
             TestLabel "4" $ TestCase $ reducesTo irel [3,0,-1]  [4]
           ]

timeout_time = 2000000 -- microseconds
timeoutAssertion :: (Eq a) => IO a -> IO ()
timeoutAssertion asst = do
  r <- System.Timeout.timeout timeout_time asst
  assertBool "timed out" (r /= Nothing)

iReduceTests2 =
  TestList [ TestLabel "1" $ TestCase $ timeoutAssertion $ reducesTo irel [0,2,0,1]   [0,1] ]

test1 = TestLabel "test1" $ TestCase (assertEqual "lose" 1 2 )
test2 = TestCase (assertEqual "win" 1 1 )

tests = TestList [ ]

