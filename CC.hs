{-# LANGUAGE TypeSynonymInstances #-}

module CC where

import Data.Function (on)
import Data.Maybe (fromMaybe)


------------
-- Syntax --
------------

type Dim  = String
type Tag  = String
type Var  = String

-- choice calculus expressions
data CC a =
    Str a [CC a]         -- subexpressions
  | Dim Dim [Tag] (CC a) -- dimension declaration
  | Chc Dim [CC a]       -- choice branching
  | Abs Var (CC a)       -- sharing abstraction
  | App (CC a) (CC a)    -- sharing application
  | Ref Var              -- variable reference
  deriving Eq

class Data a where
  showData :: a -> String
  (<.>) :: CC a -> CC a -> CC a
  (<.>) = App


------------------------
-- Smart Constructors --
------------------------

leaf :: a -> CC a
leaf a = Str a []

(@@) :: CC a -> CC a -> CC a
(@@) = App
infixl 1 @@


----------------------
-- Helper functions --
----------------------

subs :: CC a -> [CC a]
subs (Str _ es)  = es
subs (Dim _ _ e) = [e]
subs (Chc _ as)  = as
subs (Abs _ e)   = [e]
subs (App l r)   = [l,r]
subs (Ref _)     = []

swap :: CC a -> [CC a] -> CC a
swap (Str a _)   es    = Str a es
swap (Dim d t _) [e]   = Dim d t e
swap (Chc d _)   as    = Chc d as
swap (Abs v _)   [e]   = Abs v e
swap (App _ _)   [l,r] = App l r
swap (Ref v)     []    = Ref v

mapSubs :: (CC a -> r) -> CC a -> [r]
mapSubs f = map f . subs

tranSubs :: (CC a -> CC a) -> CC a -> CC a
tranSubs f e = swap e (mapSubs f e)


---------------------
-- Static Analysis --
---------------------

-- are all abstractions resolvable in this expression?
badAbsFree :: CC a -> Bool
badAbsFree = outer . reduce []
  where outer (Abs _ e) = outer e
        outer e         = inner e
        inner (Abs _ _) = False
        inner e         = all inner (subs e)

-- full beta reduction
reduce :: Map Var (CC a) -> CC a -> CC a
reduce m (App l r) = case reduce m l of
                       (Abs v l') -> reduce ((v,reduce m r):m) l'
                       l'         -> App l' (reduce m r)
reduce m (Abs v l) = Abs v $ reduce ((v,Ref v):m) l
reduce m e@(Ref v) = fromMaybe e (lookup v m)
reduce m e         = tranSubs (reduce m) e

red = reduce []

------------
-- Values --
------------

-- a choice calculus value, either a closure or a non-abstraction
data Value a = Value (CC a)
             | Closure (Env a) (CC a)
             deriving Eq

-- closure environment
type Env a = Map Var (Semantics a)

value :: Value a -> CC a
value (Value     e) = e
value (Closure _ e) = e


---------------
-- Semantics --
---------------

type Map k v = [(k,v)]

type Semantics a = Map Decision (Value a)

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq
type Decision = [QTag]

-- computing the semantics function
sem :: Data a => CC a -> Semantics a
sem = variants []

-- to replace the \Pi function in the TOSEM paper
foldSem :: [Map [a] b] -> Map [a] [b]
foldSem []     = [([],[])]
foldSem (v:vs) = [(qs++qs',e:es) | (qs,e) <- v, (qs',es) <- foldSem vs]

-- choice elimination
choiceElim :: Dim -> Int -> CC a -> CC a
choiceElim d i (Chc d' es)    | d == d' = es !! i
choiceElim d _ e@(Dim d' _ _) | d == d' = e
choiceElim d i e = tranSubs (choiceElim d i) e

-- the V function in the TOSEM paper (significantly changed)
variants :: Data a => Env a -> CC a -> Semantics a
variants m e@(Ref v)   = fromMaybe [([],Value e)] (lookup v m)
variants m e@(Abs _ _) = [([],Closure m e)]
variants m (App l r)   = variants m l `compose` variants m r -- see NOTE below
variants m (Dim d ts e) = do
    (t,i)  <- zip ts [0..]
    (qs,p) <- variants m (choiceElim d i e)
    return (Q d t : qs, p)
variants m e = do
    (qs,ps) <- foldSem (mapSubs (variants m) e)
    return (qs, Value (swap e (map value ps)))

-- NOTE: For testing/proving the composition invariant below, we may want to
--       exclude the App case since the invariant is tautological otherwise.
--       By exploiting the invariant in this way, however, we can avoid choice
--       capture by simply resolving choices before they are substituted.


---------------------------
-- Semantics Composition --
---------------------------

-- composition invariant
invariant :: (Eq a,Data a) => CC a -> CC a -> Bool
invariant l r = sem l `compose` sem r == sem (App l r)

-- semantics composition
compose :: Data a => Semantics a -> Semantics a -> Semantics a
compose ls rs = do
    (dl,lp) <- ls
    case lp of
      Closure m (Abs v l) -> [(dl++dn,n) | (dn,n) <- variants ((v,rs):m) l]
      -- language-specific composition
      Value l -> [(dl++dr, Value (l <.> value r)) | (dr,r) <- rs]


--------------------
-- Data Instances --
--------------------

treeMerge :: a -> (a -> a -> a) -> CC a -> CC a -> CC a
treeMerge d f (Str a as) (Str b bs) =
    Str (f a b) $ take (on max length as bs)
    $ on (zipWith (treeMerge d f)) (++ map leaf (repeat d)) as bs
treeMerge _ _ a b = App a b

instance Data Int where
  showData = show
  (<.>) = treeMerge 0 (+)

instance Data Integer where
  showData = show
  (<.>) = treeMerge 0 (+)

instance Data String where
  showData = id
  (<.>) = treeMerge "" (++)
