
import Data.List (mapAccumL)

{-
data Tree a = Node a (Tree a) (Tree a) | Leaf
  deriving (Eq,Show)

label :: Int -> Tree a -> (Int, Tree (Int,a))
label i Leaf         = (i,Leaf)
label i (Node a l r) = (k, Node (i,a) l' r')
  where (j,l') = label (i+1) l
        (k,r') = label j r
-}

data Tree a = Node a [Tree a]
  deriving (Eq,Show)

pre :: Int -> Tree a -> (Int, Tree (Int,a))
pre i (Node a ts) = (j, Node (i,a) us)
  where (j,us) = mapAccumL pre (i+1) ts

post :: Int -> Tree a -> (Int, Tree (Int,a))
post i (Node a ts) = (j+1, Node (j,a) us)
  where (j,us) = mapAccumL post i ts
