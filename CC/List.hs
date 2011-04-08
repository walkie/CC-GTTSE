{-# LANGUAGE DeriveDataTypeable #-}

module CC.List where

import Control.Applicative   (Applicative (pure,(<*>)))
import Control.Monad         (ap)
import Data.Generics (Data,Typeable)

import CC.Syntax
import CC.Semantics
import CC.Pretty


-----------------------
-- Variational Lists --
-----------------------

type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a) 
  deriving (Eq,Data,Typeable)

instance Compose (List a) where
  (<.>) = cat

{-
instance Compose a => Compose (List a) where
  Cons a l <.> Cons b r = Cons (a <.> b) (l <.> r)
  Empty <.> r = r
  l <.> Empty = l
-}

-- smart constructor for singleton lists
singleton :: a -> List a
singleton a = Cons a Empty

-- smart constructor for consing to a vlist directly
cons :: a -> VList a -> VList a
cons a v = Obj (Cons a (VList v))

-- an empty vlist
empty :: VList a
empty  = Obj Empty

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


----------------------
-- Variational Text --
----------------------

{-
type VText = VList T.Text

instance Compose T.Text where
  (<.>) = T.append

append :: List T.Text -> T.Text -> List T.Text
append Empty      t = Cons t Empty
append (Cons u l) t = Cons (T.append u t) l
append (VList e)  t = VList (fmap (`append` t) e)

appendS :: List T.Text -> String -> List T.Text
appendS l = append l . T.pack
-}


---------------
-- Instances --
---------------

instance Monad List where
  return = singleton
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
  show Empty      = ""
  show (Cons a l) = show a ++ show l
  show (VList e)  = show e
