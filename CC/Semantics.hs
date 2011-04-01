
module CC.Semantics where

import Data.Generics (Data)

import CC.Syntax
import CC.Static
import CC.Pretty

-----------
-- Types --
-----------

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq
type Dec  = [QTag]

-- CC semantics (can represent partial or full)
type Sem a = Map Dec (Value a)

-- a choice calculus value, either a closure or a plain expression
data Value a = Plain a
             | Close (Env a) Var (V a)
             deriving Eq

-- closure environment
type Env a = Map Var (Sem a)

value :: Value a -> V a
value (Plain     e) = Obj e
value (Close _ v e) = Abs v e


---------------
-- Semantics --
---------------

-- computing the semantics function
sem :: (Compose a, Data a) => V a -> Sem a
sem = vary []

-- partial semantics composition (bowtie in FSE-11 paper)
compose :: (Compose a, Data a) => Sem a -> Sem a -> Sem a
compose sl sr = concatMap (composeR sr) sl

composeR :: (Compose a, Data a) => Sem a -> (Dec, Value a) -> Sem a
composeR sr (ql,Plain l) = [(ql++qr, Plain (l <.> r)) | (qr, Plain r) <- sr]
composeR sr (ql,Close m v r)   = [(ql++qr, vr) | (qr,vr) <- vary ((v,sr):m) r]

-- partial semantics (V_rho in FSE-11 paper)
vary :: (Compose a, Data a) => Env a -> V a -> Sem a
vary m (Ref v)     = maybe (unboundVar v) id (lookup v m)
vary m (Abs v e)   = [([], Close m v e)]
vary m (App l r)   = vary m l `compose` vary m r
vary m (Let v b u) = [([], Close m v u)] `compose` vary m (ccfix @@ Abs v b)
vary m (Shr v b u) = do
    (qb,vb) <- vary m b
    (qu,vu) <- [([], Close m v u)] `compose` [([],vb)]
    return (qb++qu, vu)
vary m (Dim d ts e) = do
    (t,i)  <- zip ts [0..]
    (qs,p) <- vary m (elim d i e)
    return (Q d t : qs, p)
vary _ (Chc d _) = unboundDim d
vary m e@(Obj _) = do
    (qs,ps) <- foldSem $ ccQ (vary m) e
    let Obj a = swap e (map value ps)
    return (qs, Plain a)


foldSem :: [Map [a] b] -> Map [a] [b]
foldSem []     = [([],[])]
foldSem (v:vs) = [(qs++qs',e:es) | (qs,e) <- v, (qs',es) <- foldSem vs]

-- choice elimination
elim :: Data a => Dim -> Int -> V a -> V a
elim d i   (Chc d' es)  | d == d' = es !! i
elim d _ e@(Dim d' _ _) | d == d' = e
elim d i e = ccT (elim d i) e


------------
-- Errors --
------------

unboundVar v = error $ "Unbound variable: " ++ v
unboundDim d = error $ "Unbound dimension: " ++ d


---------------
-- Instances --
---------------

instance Show QTag where
  show (Q d t) = showSel d t

instance Show a => Show (Value a) where
  show (Plain e)     = show e
  show (Close m v e) = showEnv m ++ ':' : showAbs v (show e)
