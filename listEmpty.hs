
data Empty = Empty
data Nonempty = Nonempty

data MyList a e where
  Nil :: MyList a Empty
  Cons :: a -> MyList a e -> MyList a Nonempty

isEmpty :: MyList a e -> Bool
isEmpty (Cons _ _) = False
isEmpty Nil        = True

-- list2mylist :: [a] -> MyList b
-- list2mylist (h:t) = Cons h (list2mylist t)
-- list2mylist [] = Nil

-- cut :: MyList a Nonempty -> (a, MyList a e)
-- cut (Cons a b) = (a, b)

hd :: MyList a Nonempty -> a
hd (Cons a _) = a

-- tl :: MyList a Nonempty -> MyList a b
-- tl (Cons _ a) = a
