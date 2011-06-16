{-# LANGUAGE DeriveDataTypeable #-}

module Examples.List where

import Control.Applicative (Applicative (pure,(<*>)))
import Control.Monad       (ap)
import Data.Generics       (Data,Typeable)
import List                (intersperse)

import CC.Syntax
import CC.Pretty
import CC.Semantics
-- import CC.Share

import Examples.Names


default (Int)


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
--

-- smart constructor for singleton lists
single :: a -> List a
single a = Cons a Empty

-- from an ordinary Haskell list to a List value
toList :: [a] -> List a
toList = foldr Cons Empty

list = toList

-- from a *plain* List value to a Haskell list
fromList :: List a -> [a]
fromList Empty      = []
fromList (Cons a l) = a : fromList l

-- lift Haskell list functions to Lists
liftL :: ([a] -> b) -> (List a -> b)
liftL = (. fromList)

-- concatenate two lists

--
-- working with variational lists
--

-- smart constructors and functions for consing to a vlist directly
vsingle :: a -> VList a
vsingle = plain . single

vList :: [a] -> VList a
vList = plain . toList

vlist = plain . list

cons :: a -> VList a -> VList a
cons a v = plain (Cons a (VList v))

vcons :: V a -> VList a -> VList a
vcons vx vxs = do {x <- vx; cons x vxs}

-- an empty vlist
vempty :: VList a
vempty  = plain Empty

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


--
-- variational list functions
--

-- foldr
fold :: (a -> b -> b) -> b -> List a -> V b
fold _ b Empty      = plain b
fold f b (Cons a l) = fmap (f a) (fold f b l)
fold f b (VList vl) = vl >>= fold f b
-- fold f b (VList vl) = liftV (fold f b) vl


-- length
len :: List a -> V Int
len = fold (\_ s->succ s) 0
{-
len :: List a -> V Int
len Empty       = plain 0
len (Cons _ xs) = fmap (+1) (len xs)
len (VList vl)  = vl >>= len
-- len (VList vl)  = liftV len vl
-}
vlen :: VList a -> V Int
vlen = liftV len


-- sum
sumL :: List Int -> V Int
sumL = fold (+) 0
{-
sumL :: List Int -> V Int
sumL Empty       = plain 0
sumL (Cons x xs) = fmap (x+) (sumL xs)
sumL (VList vl)  = vl >>= sumL
-}
vsum :: VList Int -> V Int
vsum = liftV sumL

-- nth
nth :: Int -> List a -> V a
nth _ Empty       = undefined
nth 1 (Cons x _)  = plain x
nth n (Cons _ xs) = nth (n-1) xs
nth n (VList vl)  = vl >>= nth n

vnth ::  Int -> VList a -> V a
vnth n = liftV (nth n)

-- filter
filterL :: (a -> Bool) -> List a -> List a
filterL p Empty       = Empty
filterL p (Cons x xs) | p x       = Cons x (filterL p xs)
                      | otherwise = filterL p xs
filterL p (VList vl)  = VList (fmap (filterL p) vl)

vfilter :: (a -> Bool) -> VList a -> VList a
vfilter p = fmap (filterL p)


-- map
mapL :: (a -> b) -> List a -> List b
mapL = fmap

vmap :: (a -> b) -> VList a -> VList b
-- vmap f = fmap (mapL f)
vmap = fmap . fmap


-- elem
member :: Eq a => a -> List a -> V Bool
member _ Empty                   = plain False
member x (Cons y ys) | x==y      = plain True
                    | otherwise = member x ys
member x (VList vl)              = vl >>= member x

vmember :: Eq a => a -> VList a -> V Bool
vmember x = liftV (member x)


-- nub
rdup :: Eq a => List a -> List a
rdup Empty       = Empty
rdup (Cons x xs) = Cons x (filterL (/=x) (rdup xs))
rdup (VList vl)  = VList (fmap rdup vl)

vrdup :: Eq a => VList a -> VList a
vrdup = fmap rdup


-- append
cat :: List a -> List a -> List a
cat Empty      r = r
cat (Cons a l) r = Cons a (l `cat` r)
cat (VList vl) r = VList (fmap (`cat` r) vl)

vcat :: VList a -> VList a -> VList a
vcat l r = plain $ cat (VList l) (VList r)

-- vapp :: List a -> List a -> List a
-- vapp = cat
-- 
-- vappVV :: VList a -> VList a -> VList a
-- vappVV = vcat


-- reverse
rev :: Eq a => List a -> List a
rev Empty       = Empty
rev (Cons x xs) = rev xs `cat` single x 
rev (VList vl)  = VList (fmap rev vl)

vrev :: Eq a => VList a -> VList a
vrev = fmap rev


-- zip
zipL :: List a -> List b -> List (a,b)
zipL Empty       ys          = Empty
zipL xs          Empty       = Empty 
zipL (Cons x xs) (Cons y ys) = Cons (x,y) (zipL xs ys)
zipL (VList vl)  ys          = VList (fmap (`zipL` ys) vl)
zipL xs          (VList vl') = VList (fmap (xs `zipL`) vl')

vzip :: VList a -> VList b -> VList (a,b)
vzip vl vl' = plain $ zipL (VList vl) (VList vl')



--------------
-- Examples --
--------------

-- generic example definition
--


-- lists
--
e1 = dimA $ chcA [vempty, 1 `cons` vempty]
e2 = dimB $ chcB [vempty, 2 `cons` e1]

vx  = chc'A [1,2]
vy  = chc'A [3,4]
vyB = chc'B [3,4]
vl  =  vList [8,9]

v1  = dimA $ vx `vcons` vl
v2  = dimA $ vx `vcons` (vy `vcons` vl)
v2B = dimA $ dimB $ vx `vcons` (vyB `vcons` vl)
v2C = dimA $ vx `vcons` (dimB $ vyB `vcons` vl)

-- vnubV $ chc'A [8,9] `vcons` vl
-- A<[8;9],[9;8]>


data Food = Steak | Pasta | Beer | Fries | Cake
          | Sherry
            deriving (Eq,Show,Data,Typeable)
    
[y,n] = ["yes","no"]

--  Too complicated and too little variational:
--
-- cMain = chc' "Main" 
-- 
-- main   = cMain [Steak,Pasta]
-- drink  = cMain [Beer,Wine]
-- dessert = aDim' "Dessert" ["c","f"] [Cake,Fruit]
-- 
-- special = aDim "Drink" [y,n] [cMain [Beer,Wine],dessert] `vcons` vempty
-- 
-- menu = Dim "Main" ["m","p"] $ main `vcons` special


-- menu = Dim "Main" ["m","p"] $ 
--        Dim "Drink" [y,n] $
--             main `vcons` drink

-- Too many length variation, no nested dimension
--
-- main = chc "Main" $ map vList [[Steak,Fries],[Pasta]]
-- dessert = aDim "Dessert" [y,n] [vsingle Cake,vempty]
-- dessert' = chc "Dessert" [vsingle Cake,vempty]
-- 
-- menu = Dim "Main" ["m","p"] $ main `vcat` dessert
-- 
-- menu' = Dim "Main" ["m","p"] $ 
--         Dim "Dessert" [y,n] 
--             main `vcat` dessert'
-- 

dessert :: VList Food
dessert = aDim "Dessert" [y,n] [vsingle Cake,vempty]

menu :: VList Food
menu = aDim "Main" ["meat","pasta"] 
             [vlist [Steak,Fries],Pasta `cons` dessert]

-- data Drink = Martini | Sherry
--              deriving (Eq,Show,Data,Typeable)

aperitif :: VList Food
aperitif = aDim "Drink" [y,n] [vsingle Sherry,vempty]


---------------
-- Instances --
---------------

instance Functor List where
  fmap _ Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f (VList vl)  = VList $ fmap (fmap f) vl

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

-- instance Functor List where
--   fmap f e = e >>= return . f

-- instance Show a => Show (List a) where
--   show Empty      = "[]"
--   show (Cons a l) = show a ++ ":" ++ show l
--   show (VList e)  = show e
-- 

instance Show a => Show (List a) where
  show (VList e)  = show e
  show Empty      = "[]"
  show vl         = '[':show' vl++"]"

show' Empty           = "[]"
show' (Cons x Empty)  = show x
show' (Cons x vl)     = show x++';':show' vl
show' (VList (Obj l)) = show' l
show' (VList e)       = show e

