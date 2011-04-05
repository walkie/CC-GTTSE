{-# LANGUAGE DeriveDataTypeable #-}

module CC.Tree where

import Control.Applicative   (Applicative (pure,(<*>)))
import Control.Monad         (ap,guard)
import Data.Generics

import CC.Syntax
import CC.Semantics
import CC.Pretty


-----------------------
-- Variational Trees --
-----------------------

type VTree a = V (Tree a)

data Tree a = Node a [Tree a]
            | VTree (VTree a)
  deriving (Eq,Data,Typeable)

instance Compose a => Compose (Tree a) where
  Node a as <.> Node b bs = Node (a <.> b) (zipWith (<.>) as bs ++ drop n as ++ drop n bs)
    where n = min (length as) (length bs)

-- smart constructor for leaf nodes
leaf :: a -> Tree a
leaf a = Node a []

addChildren :: [Tree a] -> Tree a -> Tree a
addChildren cs (Node a ts) = Node a (ts++cs)
addChildren cs (VTree e)   = VTree (fmap (addChildren cs) e)

-- is this tree plain?
isPlain :: Tree a -> Bool
isPlain (Node _ ts)     = all isPlain ts
isPlain (VTree (Obj t)) = isPlain t
isPlain _               = False

-- strip superfluous Obj constructors from a *plain* tree
stripObj :: Tree a -> Tree a
stripObj (Node a ts)     = Node a (map stripObj ts)
stripObj (VTree (Obj t)) = stripObj t
stripObj _ = error "stripObj: Tree is not plain."


----------------------------------
-- To/From Generic String Trees --
----------------------------------

-- Cram an arbitrary data type into a variational tree.
toStringTree :: Data a => a -> Tree String
toStringTree = other `extQ` leaf
  where other a = Node (showConstr (toConstr a)) (gmapQ toStringTree a)

-- Pull original data type out of a *plain* choice calculus expression.
fromStringTree :: Data a => Tree String -> Maybe a
fromStringTree = (other `extR` string) . stripObj
  where string (Node s []) = Just s
        other  (Node s es) = result
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


---------------
-- Instances --
---------------

instance Monad Tree where
  return = leaf
  VTree e   >>= f = VTree (fmap (>>= f) e)
  Node a ts >>= f = let ts' = map (>>= f) ts in
                    case f a of
                      Node b us -> Node b (us ++ ts')
                      VTree e   -> VTree $ fmap (addChildren ts') e

instance Applicative Tree where
  pure  = return
  (<*>) = ap

instance Functor Tree where
  fmap f e = e >>= return . f

instance Show a => Show (Tree a) where
  show (Node a []) = show a
  show (Node a ts) = show a ++ (braces . commas id) (map show ts)
  show (VTree e)   = show e
