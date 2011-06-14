{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Tree where

import Data.Generics (Data,Typeable)
import Data.Maybe (fromJust)

import CC.Syntax
import CC.Static
import CC.Semantics
import CC.Pretty
import CC.Tree


---------------------------------
-- Generic String Tree Example --
---------------------------------

-- An arbitrary nested data type representing math expressions.
--

data Expr a = Plus (Expr a) (Expr a)
            | Term (Term a)
  deriving (Eq,Data,Typeable)

data Term a = Times (Term a) (Term a)
            | Paren (Expr a)
            | Value a
  deriving (Eq,Data,Typeable)

instance Show a => Show (Expr a) where
  show (Plus l r) = show l ++ "+" ++ show r
  show (Term t)   = show t
instance Show a => Show (Term a) where
  show (Times l r) = show l ++ "*" ++ show r
  show (Paren e)   = "(" ++ show e ++ ")"
  show (Value a)   = show a

eval :: Num a => Expr a -> a
eval (Plus l r) = eval l + eval r
eval (Term t)   = term t
  where term (Times l r) = term l * term r
        term (Paren e)   = eval e
        term (Value a)   = a


-- functions for converting math expressions to/from string trees
--

toExpr :: Tree String -> Expr Int
toExpr = fromJust . fromST

fromExpr :: Expr Int -> Tree String
fromExpr = toST

toTerm :: Tree String -> Term Int
toTerm = fromJust . fromST

fromTerm :: Term Int -> Tree String
fromTerm = toST

psemExpr t = pretty [(d,toExpr s) | (d,s) <- sem t]
psemTerm t = pretty [(d,toTerm s) | (d,s) <- sem t]


-- handy smart constructor
val :: a -> Expr a
val = Term . Value

-- a math expression: 1+2*(3+4)
e1 :: Expr Int
e1 = val 1 `Plus` Term (Value 2 `Times` Paren (val 3 `Plus` val 4))

-- 15
e1' = eval e1

-- convert the expression into a variational string tree
ve1 :: VTree String
ve1 = Obj (fromExpr e1)

-- replace every value with a choice between the original value x or 10*x
ve2 :: VTree String
ve2 = dimA $ ve1 >>= Obj . valToChc
  where times10 e = fromTerm $ Value 10 `Times` toTerm e
        valToChc e@(Node "Value" _) = VTree $ Chc "A" [Obj e, Obj (times10 e)]
        valToChc (Node s ts) = Node s (map valToChc ts)

{- in GHCi:

*Examples.Tree> psemExpr ve1
[]  =>  1+2*(3+4)

*Examples.Tree> psemExpr ve2
[A.a]  =>  1+2*(3+4)
[A.b]  =>  10*1+10*2*(10*3+10*4)

-}
