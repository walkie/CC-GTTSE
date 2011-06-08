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

instance Data a => VT (Tree a) where
  cleanup (Node a ts)     = Node a (map cleanup ts)
  cleanup (VTree (Obj t)) = cleanup t
  cleanup _ = error "cleanup: Tree is not plain."

-- smart constructor for leaf nodes
leaf :: a -> Tree a
leaf a = Node a []

addChildren :: [Tree a] -> Tree a -> Tree a
addChildren cs (Node a ts) = Node a (ts++cs)
addChildren cs (VTree e)   = VTree (fmap (addChildren cs) e)


----------------------------------
-- To/From Generic String Trees --
----------------------------------

-- Cram an arbitrary data type into a variational tree.
toST :: Data a => a -> Tree String
toST = other `extQ` leaf
  where other a = Node (showConstr (toConstr a)) (gmapQ toST a)

-- Pull original data type out of a *plain* choice calculus expression.
fromST :: Data a => Tree String -> Maybe a
fromST = (other `extR` string) . cleanup
  where string (Node s []) = Just s
        other  (Node s es) = result
          where unM = undefined :: m a -> a -- only used for type
                result = do c <- readConstr (dataTypeOf (unM result)) s
                            let a = fromConstr c
                            guard (glength a == length es)
                            snd (gmapAccumM (\(x:xs) _ -> (xs,fromST x)) es a)
        other _ = Nothing -- CC expression is not plain (or some other error)

-- The semantics of a variational generic string tree, automatically converting
-- plain expressions back into the original data type.
semST :: Data a => VTree String -> Sem (Maybe a)
semST t = [(d,fromST s) | (d,s) <- sem t]


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
