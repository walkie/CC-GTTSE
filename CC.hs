{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}

module CC where

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Typeable

import Debug.Trace

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

wellDim :: Map Dim Int -> CC a -> Bool
wellDim m (Dim d ts e) = wellDim ((d,length ts):m) e
wellDim m (Chc d es)   = maybe False (== length es) (lookup d m) && all (wellDim m) es
wellDim m e            = and $ mapSubs (wellDim m) e

wellRef :: [Var] -> CC a -> Bool
wellRef vs (Abs v e) = wellRef (v:vs) e
wellRef vs (Ref v)   = elem v vs
wellRef vs e         = and $ mapSubs (wellRef vs) e

-- full beta reduction
reduce :: Map Var (CC a) -> CC a -> CC a
reduce m (App l r) = case reduce m l of
                       (Abs v l') -> reduce ((v,reduce m r):m) l'
                       l'         -> App l' (reduce m r)
reduce m (Abs v l) = Abs v $ reduce ((v,Ref v):m) l
reduce m e@(Ref v) = fromMaybe e (lookup v m)
reduce m e         = tranSubs (reduce m) e

red = reduce []


-----------------
-- Type System --
-----------------

data CCT t = 
    StrT t [CCT t]
  | DimT Dim [Tag] (CCT t)
  | ChcT Dim [CCT t]
  | FunT (CCT t) (CCT t)
  | VarT Int
  deriving Eq

next :: Map String Int -> Int
next []        = 0
next ((_,i):_) = i+1

{-
swapT :: CCT t -> [CC t] -> CC t
swapT (Str a _)   es    = Str a es
swapT (Dim d t _) [e]   = Dim d t e
swapT (Chc d _)   as    = Chc d as
swapT (Abs v _)   [e]   = Abs v e
swapT (App _ _)   [l,r] = App l r
swapT (Ref v)     []    = Ref v
-}

infer :: Typeable a => Map Var Int -> CC a -> CCT TypeRep
infer m (Str a es)   = StrT (typeOf a) (map (infer m) es)
infer m (Dim d ts e) = DimT d ts ((infer m) e)
infer m (Chc d es)   = ChcT d (map (infer m) es)
infer m (Abs v e)    = FunT (VarT i) (infer ((v,i):m) e) where i = next m
infer m (App l r)    = infer m l `composeT` infer m r
infer m (Ref v)      = maybe err VarT (lookup v m) where err = error ("undefined: "++v)

composeT :: CCT t -> CCT t -> CCT t
composeT (StrT t _) _    = error "a"
composeT (DimT d ts l) r = DimT d ts (l `composeT` r) -- wrong

same :: [Int] -> Maybe Int
same []                    = Just 0
same (i:is) | all (i==) is = Just i
            | otherwise    = Nothing

maxi :: [Int] -> Int
maxi = maximum . (0:)

isZero :: Int -> Maybe Int
isZero 0 = Just 0
isZero _ = Nothing

addVar :: Var -> Map Var Int -> [Int] -> (Map Var Int,[Int])
addVar v m []     = ((v,0):m,[])
addVar v m (a:as) = ((v,a):m,as)

arity :: Map Var Int -> [Int] -> CC a -> Maybe Int
arity m as (Abs v e) = fmap (+1) (arity m' as' e) where (m',as') = addVar v m as
arity m as (App l r) = do ra <- arity m [] r
                          la <- arity m (ra:as) l
                          return (max 0 (la-1))
arity m _  (Ref v)   = lookup v m
arity m as e         = fmap maxi $ sequence (mapSubs (arity m as) e) -- >>= same

ari = arity [] []

--check e = ari e == Just 0

{-
arity :: Map Var (Maybe Int) -> [Int] -> CC a -> Maybe Int
arity m as (Str _ es) = Just 0
arity m as (App l r)  = check n r && check (n+1) l
arity m as (Abs _ e)  = check (n-1) e
arity m as e          = and (mapSubs (check n) e)
-}

{-
data Status = AllFun | NoFun | Error
  deriving Eq

allFun :: Map Var Status -> CC a -> Bool
allFun _ (Str _ _)   = False
allFun m (Dim _ _ e) = allFun m e
allFun m (Chc _ es)  = all (allFun m) es
allFun _ (Abs _ _)   = True
allFun m (App l r)   = undefined
allFun m (Ref v)     = maybe False (==AllFun) (lookup v m)

noFun :: Map Var Status -> CC a -> Bool
noFun m (Str _ es)  = all (noFun m) es
noFun m (Dim _ _ e) = noFun m e
noFun m (Chc _ es)  = all (noFun m) es
noFun _ (Abs _ _)   = False
noFun m (App l r)   = undefined
noFun m (Ref v)     = maybe False (==NoFun) (lookup v m)
-}

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
