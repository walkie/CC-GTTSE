{-# LANGUAGE DeriveDataTypeable, TupleSections #-}

module CC.Tree where

import Control.Monad (guard)
import Data.Generics

import CC.Syntax
import CC.Semantics
import CC.Pretty

-----------------------
-- Variational Trees --
-----------------------

type VTree a = V (Tree a)

data Tree a = Tree a [Tree a]
            | VTree (VTree a)
  deriving (Eq,Data,Typeable)

instance Compose a => Compose (Tree a) where
  Tree a as <.> Tree b bs = Tree (a <.> b) (zipWith (<.>) as bs ++ drop n as ++ drop n bs)
    where n = min (length as) (length bs)

-- smart constructor for leaf nodes
leaf :: a -> Tree a
leaf a = Tree a []

-- is this tree plain?
isPlain :: Tree a -> Bool
isPlain (Tree _ ts) = all isPlain ts
isPlain _ = False

-- strip superfluous Obj constructors from a *plain* tree
stripObj :: Tree a -> Tree a
stripObj (Tree a ts)     = Tree a (map stripObj ts)
stripObj (VTree (Obj t)) = stripObj t


----------------------------------
-- To/From Generic String Trees --
----------------------------------

-- Cram an arbitrary data type into a variational tree.
toStringTree :: Data a => a -> Tree String
toStringTree = other `extQ` leaf
  where other a = Tree (showConstr (toConstr a)) (gmapQ toStringTree a)

-- Pull original data type out of a *plain* choice calculus expression.
fromStringTree :: Data a => Tree String -> Maybe a
fromStringTree = (other `extR` string) . stripObj
  where string (Tree s []) = Just s
        other  (Tree s es) = result
          where unM = undefined :: m a -> a -- only used for type
                result = do c <- readConstr (dataTypeOf (unM result)) s
                            let a = fromConstr c
                            guard (glength a == length es)
                            snd (gmapAccumM (\(x:xs) _ -> (xs,fromStringTree x)) es a)
        other _ = Nothing -- CC expression is not plain

-- The semantics of a variational generic string tree, automatically converting
-- plain expressions back into the original data type.
semStringTree :: Data a => VTree String -> Sem (Maybe a)
semStringTree t = [(d,fromStringTree s) | (d,s) <- sem t]


---------------------
-- Pretty Printing --
---------------------

instance Show a => Show (Tree a) where
  show (Tree a []) = show a
  show (Tree a ts) = show a ++ (braces . commas id) (map show ts)
  show (VTree e)   = show e
