{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Haskell where

import Data.Generics

import CC.Syntax
import CC.Tree

import CC.Semantics
import CC.Static

data Def = Fun Name [Name] Exp
      -- | ...
  deriving (Eq,Show,Data,Typeable)

data Exp = App Exp Exp
         | Ref Name
         | Val Int
      -- | ...
  deriving (Eq,Show,Data,Typeable)

infixl 1 `App`

-- syntactic sugar for binary operations
op :: Name -> Exp -> Exp -> Exp
op o l r = Ref ("(" ++ o ++ ")") `App` l `App` r


-- twice example
--
xp = Fun "twice" ["x"] $ op "+" (Ref "x") (Ref "x")
yp = Fun "twice" ["y"] $ op "+" (Ref "y") (Ref "y")
xt = Fun "twice" ["x"] $ op "*" (Val 2) (Ref "x")
yt = Fun "twice" ["y"] $ op "*" (Val 2) (Ref "y")

refx = toST $ Ref "x"
refy = toST $ Ref "y"
plusx  = toST $ Ref "(+)" `App` Ref "x"
timesx = toST $ Ref "(*)" `App` Val 2

xy :: ST -> ST
xy t | t == refx = VTree $ chc' "Par" [refx, refy]
     | otherwise = t

pt :: ST -> ST
pt t | t == plusx = VTree $ chc' "Impl" [plusx, timesx]
     | otherwise  = t

twice :: VST
twice = Dim "Par" ["x","y"]
      $ Dim "Impl" ["plus","times"]
      $ Obj $ everywhere (mkT xy) $ everywhere (mkT pt) (toST xp)


