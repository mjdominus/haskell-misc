
extend1 :: ([a] -> a) -> [a] -> [a]
extend1 f init = init ++ rest init
  where rest ls = let next = f ls
                   in next : rest (ls ++ [next])

--------------------------------------`--------------------------

nonSum' init = init ++ sieve init [1+(maximum init)..]
  where sieve base (q:qs) | elem q forbidden = sieve base qs
                          | otherwise = q : sieve (base ++ [q]) qs
         where
           forbidden = [ x+y | x<-base, y<-base, x<=y ]

----------------------------------------------------------------

class Functor w => Comonad w where
  extract :: w a -> a
  embed :: w a -> w (w a)
  (>==) :: w a -> (w a -> b) -> w b
  c >== f  =  fmap f (embed c)

extend :: Comonad w => (w a -> a) -> w a -> w a
extend = undefined

instance Comonad [] where
  extract = undefined
  embed [] = [[]]
  embed ls = ls : (embed $ tail ls)

tails [] = [[]]
tails ls = ls : (tails $ tail ls)
