
module CC.Semantics where

import Data.Generics (Data)

import CC.Syntax
import CC.Static
import CC.Pretty
import CC.LambdaCalculus

-----------
-- Types --
-----------

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq
type Dec  = [QTag]

-- CC semantics
type Sem a = Map Dec a

-- CC partial semantics
type PSem a = Map Dec (Value a)

-- a choice calculus value, either a closure or a plain expression
data Value a = Plain a
             | Close (Env a) Var (V a)
             deriving Eq

-- closure environment
type Env a = Map Var (PSem a)

value :: Value a -> V a
value (Plain     e) = Obj e
value (Close _ v e) = Abs v e


---------------
-- Semantics --
---------------

-- computing the semantics function
sem :: (Compose a, Data a) => V a -> Sem a
sem e = [(d,p) | (d,Plain p) <- vary [] e]

-- partial semantics composition (bowtie in FSE-11 paper)
compose :: (Compose a, Data a) => PSem a -> PSem a -> PSem a
compose sl sr = concatMap (composeR sr) sl

composeR :: (Compose a, Data a) => PSem a -> (Dec, Value a) -> PSem a
composeR sr (ql,Plain l) = [(ql++qr, Plain (l <.> r)) | (qr, Plain r) <- sr]
composeR sr (ql,Close m v r)   = [(ql++qr, vr) | (qr,vr) <- vary ((v,sr):m) r]

-- partial semantics (V_rho in FSE-11 paper)
vary :: (Compose a, Data a) => Env a -> V a -> PSem a
vary m (Ref v)     = maybe (unboundVar v) id (lookup v m)
vary m (Abs v e)   = [([], Close m v e)]
vary m (App l r)   = vary m l `compose` vary m r
vary m (Let v b u) = [([], Close m v u)] `compose` vary m (fix @@ Abs v b)
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
elim d i   (Chc d' es)  | d == d' = elim d i (es !! i)
elim d _ e@(Dim d' _ _) | d == d' = e
elim d i e = ccT (elim d i) e


------------
-- Errors --
------------

unboundVar v = error $ "Unbound variable: " ++ v
unboundDim d = error $ "Unbound dimension: " ++ d


---------------------
-- Pretty Printing --
---------------------

-- pretty print a semantics
pretty :: Show a => Sem a -> IO ()
pretty = putStr . showSem

-- short cut for computing the semantics and pretty printing it
psem :: (Show a, Compose a, Data a) => V a -> IO ()
psem = pretty . sem


---------------
-- Instances --
---------------

instance Show QTag where
  show (Q d t) = showSel d t

instance Show a => Show (Value a) where
  show (Plain e)     = show e
  show (Close m v e) = showEnv m ++ ':' : showAbs v (show e)
