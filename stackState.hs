
data State s a = State (s -> (s, a))
app (State st) = st

instance Functor (State s) where
  fmap f (State st) = State $ \s -> let (s', a) = st s in (s', f a)

instance Monad (State s) where
  return a = State $ \s -> (s, a)
  (State st) >>= f = State $ \s -> let (s', a) = st s in app (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put v = State $ \_ -> (v, ())

mutate :: (s -> s) -> State s ()
mutate f = State $ \s -> (f s, ())

extract :: (s -> a) -> State s a
extract f = State $ \s -> (s, f s)

join2 op s t = State $ \i -> let (i', a)  = app s i
                                 (i'', b) = app t i' in (i'', op a b)


-- ----------------------------------------------------------------
-- state stack stuff
pop :: State [a] a
pop = State $ \(h:t) -> (t, h)

push :: a -> State [a] ()
push v = mutate (v :)

test = do push 3;
          push 4;
          a <- pop;
          b <- pop;
          return (a - b);

runIt mote = snd $ app mote []


