
module CC where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)


------------
-- Syntax --
------------

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


------------------------
-- Smart Constructors --
------------------------

leaf :: Data -> CC
leaf a = Str a []

(@@) :: CC -> CC -> CC
(@@) = App
infixl 1 @@

str :: Int -> [CC] -> CC
str = Str . show

lef :: Int -> CC
lef = leaf . show


----------------------
-- Helper functions --
----------------------

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


---------------------
-- Static Analysis --
---------------------

-- are all abstractions resolvable in this expression?
badAbsFree :: CC -> Bool
badAbsFree = outer . reduce []
  where outer (Abs _ e) = outer e
        outer e         = inner e
        inner (Abs _ _) = False
        inner e         = all inner (subs e)

-- full beta reduction
reduce :: Map Var CC -> CC -> CC
reduce m (App l r) = case reduce m l of
                       (Abs v l') -> reduce ((v,reduce m r):m) l'
                       l'         -> App l' (reduce m r)
reduce m (Abs v l) = Abs v $ reduce ((v,Ref v):m) l
reduce m e@(Ref v) = fromMaybe e (lookup v m)
reduce m e         = tranSubs (reduce m) e


------------
-- Values --
------------

-- a choice calculus value, either a closure or a non-abstraction
data Value = Value CC | Closure Env CC deriving Eq

-- closure environment
type Env = Map Var Semantics

value :: Value -> CC
value (Value     e) = e
value (Closure _ e) = e


---------------
-- Semantics --
---------------

type Map k v = [(k,v)]

type Semantics = Map Decision Value

-- qualified tags and decisions
data QTag = Q Dim Tag deriving Eq
type Decision = [QTag]

-- computing the semantics function
sem :: CC -> Semantics
sem = variants []

-- the \Pi function in the TOSEM paper
foldSem :: [Map [a] b] -> Map [a] [b]
foldSem []     = [([],[])]
foldSem (v:vs) = [(qs++qs',e:es) | (qs,e) <- v, (qs',es) <- foldSem vs]

-- choice elimination
choiceElim :: Dim -> Int -> CC -> CC
choiceElim d i (Chc d' es)    | d == d' = es !! i
choiceElim d _ e@(Dim d' _ _) | d == d' = e
choiceElim d i e = tranSubs (choiceElim d i) e

-- the V function in the TOSEM paper (significantly changed)
variants :: Env -> CC -> Semantics
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
invariant :: CC -> CC -> Bool
invariant l r = sem l `compose` sem r == sem (App l r)

-- semantics composition
compose :: Semantics -> Semantics -> Semantics
compose ls rs = do
    (dl,lp) <- ls
    case lp of
      Closure m (Abs v l) -> [(dl++dn,n) | (dn,n) <- variants ((v,rs):m) l]
      -- following should really be an error, but we'll do our best anyway
      Value l -> [(dl++dr,Value (App l (value r))) | (dr,r) <- rs]


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

instance Show Value where
  show (Value e) = show e
  show (Closure m e) = env ++ ':' : show e
    where env = "[" ++ commas id (map entry m) ++ "]"
          entry (v,p) = "(" ++ var v ++ "," ++ show p ++ ")"

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

psem :: CC -> IO ()
psem = mapM_ putStrLn . map row . sem
  where row (qs,e) = "[" ++ commas id (map show qs) ++ "]" ++ "  =>  " ++ show e


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
