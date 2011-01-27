{-# LANGUAGE TypeSynonymInstances #-}
module CC where

import Data.Function (on)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)

--
-- Syntax
--

type Data = String
type Dim  = String
type Tag  = String
type Var  = String

-- choice calculus expressions
data CC =
    Str Data [CC]    -- subexpressions
  | Dim Dim [Tag] CC -- dimension declaration
  | Chc Dim [CC]     -- choice branching
  | Abs Var CC       -- sharing abstraction
  | App CC CC        -- sharing application
  | Ref Var          -- variable reference
  deriving Eq

leaf a = Str a []

--
-- Helper functions
--

subs :: CC -> [CC]
subs (Str _ es)  = es
subs (Dim _ _ e) = [e]
subs (Chc _ as)  = as
subs (Abs _ e)   = [e]
subs (App l r)   = [l,r]
subs (Ref _)     = []

swap :: CC -> [CC] -> CC
swap (Str a _)   es    = Str a es
swap (Dim d t _) [e]   = Dim d t e
swap (Chc d _)   as    = Chc d as
swap (Abs v _)   [e]   = Abs v e
swap (App _ _)   [l,r] = App l r
swap (Ref v)     []    = Ref v

mapSubs :: (CC -> r) -> CC -> [r]
mapSubs f = map f . subs

tranSubs :: (CC -> CC) -> CC -> CC
tranSubs f e = swap e (mapSubs f e)


--
-- Static checking
-- 

-- are all abstractions resolvable in this expression?
badAbsFree :: CC -> Bool
badAbsFree = outer . reduce []
  where outer (Abs _ e) = outer e
        outer e         = inner e
        inner (Abs _ _) = False
        inner e         = all inner (subs e)

--
-- Semantics
--

type Map k v = [(k,v)]

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq

-- semantics
type Sem t e = Map [t] e

-- composition invariant
composeInv :: (Eq t, Eq e) => (CC -> Sem t e) -> (Sem t e -> Sem t e -> Sem t e)
                           -> CC -> CC -> Bool
composeInv sem comp l r = sem l `comp` sem r == sem (App l r)

-- the \Pi function in the TOSEM paper
foldSem :: [Sem t e] -> Sem t [e]
foldSem []     = [([],[])]
foldSem (v:vs) = [(qs++qs',e:es) | (qs,e) <- v, (qs',es) <- foldSem vs]

choiceElim :: Dim -> Int -> CC -> CC
choiceElim d i (Chc d' es)    | d == d' = es !! i
choiceElim d _ e@(Dim d' _ _) | d == d' = e
choiceElim d i e = tranSubs (choiceElim d i) e

-- full beta reduction
reduce :: Map Var CC -> CC -> CC
reduce m (App l r) = case reduce m l of
                       (Abs v l') -> reduce ((v,reduce m r):m) l'
                       l'         -> App l' (reduce m r)
reduce m (Abs v l) = Abs v $ reduce ((v,Ref v):m) l
reduce m e@(Ref v) = fromMaybe e (lookup v m)
reduce m e         = tranSubs (reduce m) e


------------------------------------
-- "Partial Evaluation" Semantics --
------------------------------------

type Partial = Sem QTag Part
type Env = Map Var Partial

data Part = Value CC | Closure Env CC deriving Eq

value :: Part -> CC
value (Value     e) = e
value (Closure _ e) = e

-- partial semantics
partial :: CC -> Partial
partial = variantsP []

variantsP :: Env -> CC -> Partial
variantsP m e@(Ref v)   = fromMaybe [([],Value e)] (lookup v m)
variantsP m e@(Abs _ _) = [([],Closure m e)]
variantsP m (App l r)   = variantsP m l `composeP` variantsP m r -- see NOTE below
variantsP m (Dim d ts e) = do
    (t,i)  <- zip ts [0..]
    (qs,p) <- variantsP m (choiceElim d i e)
    return (Q d t : qs, p)
variantsP m e = do
    (qs,ps) <- foldSem (mapSubs (variantsP m) e)
    return (qs, Value (swap e (map value ps)))

-- NOTE: For testing the invariant, the App line may be commented out,
--       since it is tautological otherwise.  By exploiting this invariant,
--       however, we can avoid choice capture by simply resolving choices
--       before they are substituted.


--
-- Composing Partial Semantics
--

-- Intended invariant:
prop_composeP :: CC -> CC -> Bool
prop_composeP = composeInv partial composeP

composeP :: Partial -> Partial -> Partial
composeP ls rs = do
    (dl,lp) <- ls
    case lp of
      Closure m (Abs v l) -> [(dl++dn,n) | (dn,n) <- variantsP ((v,rs):m) l]
      -- following should really be an error, but we'll do our best anyway
      Value l -> [(dl++dr,Value (App l (value r))) | (dr,r) <- rs]


-------------------------
-- "Dynamic" semantics --
-------------------------

data DTag = Hole Var | Tag QTag deriving Eq
type Dynamic = Sem DTag CC

-- dynamic semantics
dynamic :: CC -> Dynamic
dynamic = variantsD . reduce []

variantsD :: CC -> Dynamic
variantsD (Ref v) = [([Hole v], Ref v)]
variantsD (Dim d ts e) = do
    (t,i)   <- zip ts [0..]
    (qs,e') <- variantsD (choiceElim d i e)
    return (Tag (Q d t) : qs, e')
variantsD e = do 
    (qs,es) <- foldSem (mapSubs variantsD e)
    return (qs, swap e es)


--
-- Composing Dynamic Semantics
--

-- Intended invariant:
prop_composeD :: CC -> CC -> Bool
prop_composeD = composeInv dynamic composeD

composeD :: Dynamic -> Dynamic -> Dynamic
composeD ls rs = do
    (dl,l) <- ls
    case l of
      Abs v l' -> weave rs v l' dl
      _        -> [(dl++dr, App l r) | (dr,r) <- rs]

weave :: Dynamic -> Var -> CC -> [DTag] -> Dynamic
weave rs v e (Hole w:dl) | v == w = do
    (dr,r) <- rs
    (d,e') <- weave rs v (reduce1' v r e) dl
    return (dr++d,e')
weave rs v e (t:dl) = [(t:d,e') | (d,e') <- weave rs v e dl]
weave _  _ e []     = [([],e)]

reduce1' :: Var -> CC -> CC -> CC
reduce1' v s e = e'
  where (True,e') = reduce1 v s e

reduce1 :: Var -> CC -> CC -> (Bool,CC)
reduce1 v s (Ref w) | v == w = (True,s)
reduce1 v s e                = (b,swap e (reverse es))
  where (b,es) = foldl r (False,[]) (subs e)
        r (False,acc) e = (b,f:acc) where (b,f) = reduce1 v s e
        r (True,acc)  e = (True,e:acc)


------------------------
-- "Static" semantics --
------------------------

type Static = Sem QTag CC

toStatic :: Dynamic -> Static
toStatic d = [(tags qs,e) | (qs,e) <- d]
  where tags []          = []
        tags (Hole _:ts) = tags ts
        tags (Tag  t:ts) = t : tags ts

-- static semantics
static :: CC -> Static
static e = [(qs, reduce [] e) | (qs,e) <- toStatic (variantsD e)]


--
-- Composing Static Semantics
--

-- Intended invariant:
prop_composeS :: CC -> CC -> Bool
prop_composeS = composeInv static composeS

composeS :: Static -> Static -> Static
composeS ls rs = [(dl++dr, reduce [] (App l r)) | (dl,l) <- ls, (dr,r) <- rs]


---------------------
-- Pretty Printing --
---------------------

commas c = concat . intersperse (c ",")
parens s = op "(" ++ s ++ op ")"

op  = style blue
key = style (blue ++ bold)
var = style red
dim = style green
tag = style green

instance Show Part where
  show (Value e) = show e
  show (Closure m e) = env ++ ':' : show e
    where env = "[" ++ commas id (map entry m) ++ "]"
          entry (v,p) = "(" ++ var v ++ "," ++ show p ++ ")"

instance Show DTag where
  show (Hole v) = var v
  show (Tag  q) = show q

instance Show QTag where
  show (Q d t) = tag (d ++ "." ++ t)

instance Show CC where
  show (Str a [])   = a
  show (Str a es)   = a ++ "{" ++ commas id (map show es) ++ "}"
  show (Dim d ts e) = key "dim " ++ dim d ++ op "<" ++ commas op (map tag ts) ++ op ">" ++
                      key " in " ++ show e
  show (Chc d es)   = dim d ++ op "<" ++ commas op (map show es) ++ op ">"
  show (Abs v e)    = op "\\" ++ var v ++ op ". " ++ show e
  show (App l r)    = parens (show l) ++ " " ++ parens (show r)
  show (Ref v)      = var v

psem :: (Show t, Show e) => Sem t e -> IO ()
psem rs = mapM_ putStrLn (map row rs)
  where row (qs,e) = "[" ++ commas id (map show qs) ++ "]" ++ "  =>  " ++ show e


--
-- Martin's color module (modified)
--

reset = "\27[0m"
bold  = "\27[1m"

attrFG c = "\27[3" ++ show c ++ "m"

black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

defaultColor = black ++ reset

style c s = c ++ s ++ defaultColor


--
-- Examples
--

str :: Int -> [CC] -> CC
str = Str . show

lef :: Int -> CC
lef = leaf . show

(@@) = App
infixl 1 @@

dimA = Dim "A" ["a","b"]
dimA' = Dim "A" ["c","d"]
dimB = Dim "B" ["c","d"]

xx = Abs "x" (str 1 [Ref "x", Ref "x"])
ab = dimA (Chc "A" [lef 2, lef 3])
xxab = App xx ab

x2 = dimA $ Abs "x" (str 1 [Ref "x", Chc "A" [lef 2, lef 3]])

x2' = Abs "x" (str 1 [Ref "x", dimA $ Chc "A" [lef 2, lef 3]])

-- example illustrating choice capture problem
vc = Dim "A" ["a","b"]
   $ App (Abs "x" $ Dim "A" ["c","d","e"] (Ref "x"))
         (Chc "A" [lef 1, lef 2])
vc' = reduce [] vc  -- eager reduction leads to captured choice
nobug = partial vc  -- but we avoid this problem by exploiting the composition
                    -- invariant, as described above

f = Abs "f" (App (Ref "f") (str 1 []))
id_ = Abs "x" (Ref "x")

-- > dynamic f `composeD` dynamic id_
-- [([x],(\x. x) (1))]

-- dependent dimensions examples

-- (\x.dim A<a,b> in A<1,x>) (dim B<c,d> in B<2,3>)
d1l = Abs "x" (dimA (Chc "A" [lef 1, Ref "x"]))
d1r = dimB (Chc "B" [lef 2, lef 3])
d1 = App d1l d1r

-- dim A<a,b> in (\x.A<1,x> (dim A<c,d> in A<2,3>))
d2 = dimA $ Abs "x" $ App (Chc "A" [lef 1, Ref "x"])
                          (dimA' (Chc "A" [lef 2, lef 3]))

-- (\y.dim A<a,b> in (\x.A<1,x y>) (\z.dim B<e,f> in B<z,5>)) (dim A<c,d> in A<2,3>)
d3l = Abs "y" $ dimA
    $ App (Abs "x" (Chc "A" [lef 1, App (Ref "x") (Ref "y")]))
          (Abs "z" (Dim "B" ["e","f"] (Chc "B" [Ref "z", lef 5])))
d3r = dimA' (Chc "A" [lef 2, lef 3])
d3 = App d3l d3r

test = dimA $ App (Abs "x" $ str 1 [Ref "x", Chc "A" [lef 2, lef 3]])
                  (Chc "A" [lef 2, lef 3])
