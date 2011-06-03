{-# LANGUAGE DeriveDataTypeable #-}

module CC.List where

import Control.Applicative (Applicative (pure,(<*>)))
import Control.Monad       (ap)
import Data.Generics       (Data,Typeable)

import CC.Syntax
import CC.Pretty

import CC.Semantics
import CC.Share

-----------------------
-- Variational Lists --
-----------------------

type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a) 
  deriving (Eq,Data,Typeable)

--
-- working with plain lists

-- smart constructor for singleton lists
single :: a -> List a
single a = Cons a Empty

-- from an ordinary Haskell list to a List value
toList :: [a] -> List a
toList = foldr Cons Empty

-- from a *plain* List value to a Haskell list
fromList :: List a -> [a]
fromList Empty      = []
fromList (Cons a l) = a : fromList l

-- concatenate two lists
cat :: List a -> List a -> List a
cat Empty      r = r
cat (Cons a l) r = Cons a (l `cat` r)
cat (VList e)  r = VList (fmap (`cat` r) e)

--
-- working with variational lists

-- smart constructor for consing to a vlist directly
vcons :: a -> VList a -> VList a
vcons a v = Obj (Cons a (VList v))

-- an empty vlist
vempty :: VList a
vempty  = Obj Empty

vcat :: VList a -> VList a -> VList a
vcat l r = Obj $ cat (VList l) (VList r)

-- is this list plain?
isPlain :: List a -> Bool
isPlain (Cons _ l)      = isPlain l
isPlain (VList (Obj l)) = isPlain l
isPlain _               = False

-- strip superfluous Obj constructors from a *plain* list
stripObj :: List a -> List a
stripObj (Cons a l)      = Cons a (stripObj l)
stripObj (VList (Obj l)) = stripObj l
stripObj _ = error "stripObj: List is not plain."


--------------
-- Examples --
--------------

e1 = dimA $ Chc "A" [vempty, 1 `vcons` vempty]
e2 = dimB $ Chc "B" [vempty, 2 `vcons` e1]

---------------
-- Instances --
---------------

instance Monad List where
  return = single
  Empty    >>= _ = Empty
  VList e  >>= f = VList (fmap (>>= f) e)
  Cons a l >>= f = let l' = l >>= f in
                   case f a of
                      Empty    -> l'
                      Cons b m -> Cons b (m `cat` l')
                      VList e  -> VList $ fmap (`cat` l') e

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Functor List where
  fmap f e = e >>= return . f

instance Show a => Show (List a) where
  show Empty      = "[]"
  show (Cons a l) = show a ++ ":" ++ show l
  show (VList e)  = show e
