{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Haskell where

import Data.Generics
import Data.List (intersperse)

import CC.Syntax
import CC.Semantics
import CC.Static
import CC.Zipper
import Examples.Edit

type Def = Exp -> Exp

data Exp = App Exp Exp
         | Ref Name
         | Val Int
         | Let Name [Exp] Exp Exp
      -- | ...
         | VExp (V Exp)
  deriving (Eq,Data,Typeable)

infixl 1 `App`

haskell :: Exp -> V Exp
haskell = Obj


--
-- Syntactic sugar for binary operations.
-- 

op :: Name -> Exp -> Exp -> Exp
op o l r = Ref ("(" ++ o ++ ")") `App` l `App` r

isOp :: Name -> Bool
isOp n = head n == '(' && last n == ')'

getOp :: Name -> Name
getOp ('(':o) = take (length o-1) o


--
-- Syntactic sugar for top-level definitions.
--

def :: Name -> [Name] -> Exp -> Def
def n as b = Let n (map Ref as) b

end :: Exp
end = Ref ""


--
-- Twice example.
--

xp = def "twice" ["x"] (op "+" (Ref "x") (Ref "x")) end
yp = def "twice" ["y"] (op "+" (Ref "y") (Ref "y")) end
xt = def "twice" ["x"] (op "*" (Val 2) (Ref "x")) end
yt = def "twice" ["y"] (op "*" (Val 2) (Ref "y")) end

varyPar :: V Exp -> V Exp
varyPar = Dim "Par" ["x","y"] . everywhere (mkT par)
  where par (Ref "x") = VExp $ chc' "Par" [Ref "x", Ref "y"]
        par e = e

varyImpl :: V Exp -> V Exp
varyImpl = Dim "Impl" ["plus","times"] . everywhere (mkT impl)
  where impl e@(App (App (Ref "(+)") l) r) | l == r = 
          VExp $ chc' "Impl" [e, op "*" (Val 2) r]
        impl e = e

twice  = (varyPar . varyImpl . haskell) xp
twice' = (varyImpl . varyPar . haskell) xp


---------------------
-- Pretty Printing --
---------------------

-- This is not quite perfect because let expressions nested in CC expressions
-- will be rendered as top-level expressions even if they should not be.
-- Good enough for now though.

instance Show Exp where
  show e = showTop e

showDef n as b = n ++ " " ++ args as ++ " = " ++ showExp b ++ "\n"
  where args = concat . intersperse " " . map showTerm

showTop (Let n as b c) = showDef n as b ++ showTop c
showTop e              = showExp e

showExp (App (App (Ref o) l) r) | isOp o = showExp  l ++ getOp o ++ showTerm r
showExp (App l@(App _ _) r) = showExp  l ++ " " ++ showTerm r
showExp (App l r)           = showTerm l ++ " " ++ showTerm r
showExp (Let n as b c)      = "let " ++ showDef n as b ++ "in " ++ showExp c
showExp (VExp v)            = show v
showExp t                   = showTerm t

showTerm (Ref v) = v
showTerm (Val i) = show i
showTerm e@(VExp (Chc _ _)) = showExp e
showTerm e       = "(" ++ showExp e ++ ")"
