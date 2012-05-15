{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Haskell where

import Data.Generics
import Data.List (elemIndex,intersperse)

import CC.Syntax
import CC.Semantics
import CC.Static
import CC.Zipper
import Examples.Edit

type Def = Haskell -> Haskell

type VHaskell = V Haskell

data Haskell = App Haskell Haskell
         | Var Name
         | Val Int
         | Fun Name [Haskell] Haskell Haskell
      -- | ...
         | VHaskell VHaskell
  deriving (Eq,Data,Typeable)

infixl 1 `App`

haskell :: Haskell -> V Haskell
haskell = Obj


--
-- Syntactic sugar for binary operations.
-- 

op :: Name -> Haskell -> Haskell -> Haskell
op o l r = Var ("(" ++ o ++ ")") `App` l `App` r

isOp :: Name -> Bool
isOp n = head n == '(' && last n == ')'

getOp :: Name -> Name
getOp ('(':o) = take (length o-1) o


--
-- Syntactic sugar for top-level definitions.
--

def :: Name -> [Name] -> Haskell -> Def
def n as b = Fun n (map Var as) b

end :: Haskell
end = Var ""


-- 
-- Other syntactic sugar
--

(.+) = op "+"
(.*) = op "*"

[x,y,z] = map Var ["x","y","z"]

choice d = VHaskell . chc' d 

fun n vs e = haskell $ Fun n vs e end

funs :: [VHaskell] -> VHaskell
funs = Obj . foldl append end . reverse
  where append e (Obj (Fun f as b (Var ""))) = Fun f as b e

--
-- Twice example.
--

-- twice = Dim "Par" ["x","y"]
--       $ Dim "Impl" ["plus","times"]
--       $ haskell $ Fun "twice" [v] i end
--   where v = VHaskell $ chc' "Par" [Var "x", Var "y"]
--         i = VHaskell $ chc' "Impl" [op "+" v v, op "*" (Val 2) v]
-- 
-- twice = Dim "Par" ["x","y"]
--       $ Dim "Impl" ["plus","times"]
--       $ fun "twice" [v] i
--       where v = choice "Par" [x,y]
--             i = choice "Impl" [v .+ v, Val 2 .* v]

twice = Dim "Par" ["x","y"] $
        Dim "Impl" ["plus","times"] $
        let v = choice "Par" [x,y] in 
        fun "twice" [v] (choice "Impl" [v .+ v, Val 2 .* v])

-- twice variants

xp, yp, xt, yt :: VHaskell
xp = fun "twice" [x] (x .+ x)
yp = fun "twice" [y] (y .* y)
xt = fun "twice" [x] (Val 2 .* x)
yt = fun "twice" [y] (Val 2 .* y)

-- deriving with SYB...
-- addChoice :: Haskell -> Haskell
-- addChoice (Var "x") = choice "Par" [x,y]
-- addChoice e = e
-- 
-- varyPar :: VHaskell -> VHaskell
-- varyPar = Dim "Par" ["x","y"] . everywhere (mkT addChoice)

addPar :: Haskell -> Haskell
addPar (Var "x") = choice "Par" [x,y]
addPar e = e

varyPar :: VHaskell -> VHaskell
varyPar = Dim "Par" ["x","y"] . everywhere (mkT addPar)

addImpl e@(App (App (Var "(+)") l) r) | l == r = choice "Impl" [e, Val 2 .* r]
addImpl e = e

varyImpl :: VHaskell -> VHaskell
varyImpl = Dim "Impl" ["plus","times"] . everywhere (mkT addImpl)


twice'  = (varyPar . varyImpl) xp
twice'' = (varyImpl . varyPar) xp


--
-- extending Par with another options
--

twiceZ :: VHaskell
twiceZ = extend "Par" "z" (addAlt (haskell z)) twice

twiceZ' :: VHaskell
twiceZ' = extend' "Par" "z" (addAlt (haskell z)) twice

stopTest = extend "Par" "z" (addAlt (haskell z)) twice'
  where dimPar = Dim "Par" ["x","y"]
        twice' = dimPar $
                 Dim "Impl" ["plus","times"] $
                 let v = choice "Par" [x,y] in 
                 let v' = VHaskell $ dimPar (haskell v) in
                 fun "twice" [v] (choice "Impl" [v .+ v', Val 2 .* v])

--
-- Editing Operations
--

firstFun :: Pred Haskell
firstFun (Obj (Fun _ _ _ _)) = True
firstFun _                   = False

renameRef :: Name -> Name -> Haskell -> Haskell
renameRef x y (Var x')
    | x == x'   = choice "Par" [Var x,Var y]
    | otherwise = Var x'
renameRef _ _ e = e

renamePar :: VHaskell -> Name -> Name -> VHaskell
renamePar e x y = withFallback e $ do
    (c, Obj (Fun f as b end)) <- extract firstFun e
    let as' = everywhere (mkT (renameRef x y)) as
    let b'  = everywhere (mkT (renameRef x y)) b
    return (c <@ Dim "Par" [x,y] (Obj (Fun f as' b' end)))

---------------------
-- Pretty Printing --
---------------------

-- This is not quite perfect because let expressions nested in CC expressions
-- will be rendered as top-level expressions even if they should not be.
-- Good enough for now though.

instance Show Haskell where
  show e = showTop e

showDef n as b = n ++ " " ++ args as ++ " = " ++ showHaskell b
  where args = concat . intersperse " " . map showTerm

showTop (Fun n as b (Var "")) = showDef n as b
showTop (Fun n as b c)        = showDef n as b ++ "\n" ++ showTop c
showTop e                     = showHaskell e

showHaskell (App (App (Var o) l) r) | isOp o = showHaskell  l ++ getOp o ++ showTerm r
showHaskell (App l@(App _ _) r) = showHaskell  l ++ " " ++ showTerm r
showHaskell (App l r)           = showTerm l ++ " " ++ showTerm r
showHaskell (Fun n as b c)      = "let " ++ showDef n as b ++ "in " ++ showHaskell c
showHaskell (VHaskell v)        = show v
showHaskell t                   = showTerm t

showTerm (Var v) = v
showTerm (Val i) = show i
showTerm e@(VHaskell (Chc _ _)) = showHaskell e
showTerm e = "(" ++ showHaskell e ++ ")"
