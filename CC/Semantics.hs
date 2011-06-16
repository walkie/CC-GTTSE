
module CC.Semantics where

import Data.Generics (Data)

import CC.Syntax
import CC.Static
import CC.Pretty
import CC.Zipper

-----------
-- Types --
-----------

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq
type Dec  = [QTag]

-- CC semantics
type Sem a = Map Dec a


---------------
-- Semantics --
---------------

-- variational semantics
sem :: VT a => V a -> Sem a
sem e = [(q, cleanup a) | (q, Obj a) <- vary e]

-- V in the TOSEM paper
vary :: Data a => V a -> Sem (V a)
vary (Dim d ts e) = do
    (t,i)  <- zip ts [0..]
    (qs,e') <- vary (elim d i e)
    return (Q d t : qs, e')
vary (Chc d _) = error $ "unbound dimension: " ++ d
vary e = do
    (qs,ps) <- foldSem $ ccQ vary e
    return (qs, swap e ps)

-- \Pi in the TOSEM paper (simplified)
foldSem :: [Map [a] b] -> Map [a] [b]
foldSem []     = [([],[])]
foldSem (v:vs) = [(qs++qs',e:es) | (qs,e) <- v, (qs',es) <- foldSem vs]

-- choice elimination
elim :: Data a => Dim -> Int -> V a -> V a
elim d i   (Chc d' es)  | d == d' = elim d i (es !! i)
elim d _ e@(Dim d' _ _) | d == d' = e
elim d i e = ccT (elim d i) e


---------------------
-- Pretty Printing --
---------------------

-- pretty print a semantics
pretty :: Show a => Sem a -> IO ()
pretty = putStr . showSem

-- short cut for computing the semantics and pretty printing it
psem :: (Show a, VT a) => V a -> IO ()
psem = pretty . sem


---------------
-- Instances --
---------------

instance Show QTag where
  show (Q d t) = showSel d t
