
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving Eq

{-
  a - b - c
        ` d
    ` e - f
        ` g
-}
instance Show a => Show (Tree a) where
  show Empty = "*"
  show (Node d lt rt) = unlines $
    (indent'' ((show d) ++ " - ") (showLines rt)) ++ 
    (indent'' (((phantom . show) d) ++ " ` ") (showLines lt)) where
      indent i = map (i ++)
      indent' i j (l:ls) = (i ++ l) : indent j ls
      indent'' x ls = indent' x (phantom x) ls
      phantom x = take (length x) (repeat ' ')
      showLines = lines . show


instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node d lt rt) = Node (f d) (fmap f lt) (fmap f rt)

insert :: Ord x => Tree x -> x -> Tree x
insert Empty x = Node x Empty Empty
insert (Node d lt rt) x | x > d   = Node d lt (insert rt x)
                        | x == d  = Node d lt rt
                        | x < d   = Node d (insert lt x) rt

insertAll t = foldl insert t


