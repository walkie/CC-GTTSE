{-# LANGUAGE DeriveDataTypeable #-}

module CC.List where

import Control.Applicative (Applicative (pure,(<*>)))
import Control.Monad       (ap,liftM2)
import Data.Generics       (Data,Typeable)

import CC.Syntax
import CC.Pretty


-----------------------
-- Variational Lists --
-----------------------

type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a) 
  deriving (Eq,Data,Typeable)

instance Data a => VT (List a) where
  cleanup (Cons a l)      = Cons a (cleanup l)
  cleanup Empty           = Empty
  cleanup (VList (Obj a)) = a

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
fromList (VList (Obj l)) = fromList l

-- concatenate two lists
cat :: List a -> List a -> List a
cat Empty      r = r
cat (Cons a l) r = Cons a (l `cat` r)
cat (VList e)  r = VList (fmap (`cat` r) e)

--
-- working with variational lists

-- smart constructor for consing to a vlist directly
--   (this one is strange because not all arguments are variational...)
vcons :: a -> VList a -> VList a
vcons a vl = Obj (Cons a (VList vl))
--vcons va vl = va >>= \a -> Obj (Cons a (VList vl))

infixr 5 `vcons`

-- an empty vlist
vempty :: VList a
vempty  = Obj Empty

-- concatenate two variational lists
vcat :: VList a -> VList a -> VList a
vcat l r = Obj $ cat (VList l) (VList r)

-- variational foldr (old)
vfoldr :: (a -> b -> b) -> b -> List a -> V b
vfoldr _ b Empty      = Obj b
vfoldr f b (Cons a l) = fmap (f a) (vfoldr f b l)
vfoldr f b (VList vl) = vl >>= vfoldr f b

vfoldl :: (V a -> b -> V a) -> V a -> VList b -> V a
vfoldl f a vl = vl >>= fold f a
  where 
    fold _ a Empty      = a
    fold f a (Cons b l) = fold f (f a b) l
    fold f a (VList vl) = vfoldl f a vl

vscanl :: (V a -> b -> V a) -> V a -> VList b -> VList a
vscanl f va vl = liftM2 Cons va (vl >>= scan f va)
  where 
    scan _ _  Empty      = vempty
    scan f va (Cons b l) = liftM2 Cons va' (scan f va' l) where va' = f va b
    scan f va (VList vl) = vl >>= scan f va

--------------
-- Examples --
--------------

b1  = dimB $ Chc "B" [vempty, 2 `vcons` vempty]
ab1 = dimA $ Chc "A" [vempty, 1 `vcons` b1]

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
