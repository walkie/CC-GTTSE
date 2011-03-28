
module Examples where

import CC
import Pretty

import Data.List (nub,subsequences)
import Prelude hiding (id,or)


--
-- useful pure lambda calculus functions
--

-- identity (duh)
id  = Abs "x" (Ref "x")

-- function composition
dot :: CC String
dot = Abs "f" $ Abs "g" $ Abs "x"
    $ Ref "f" @@ (Ref "g" @@ Ref "x")

-- church numerals
num :: Int -> CC String
num n = Abs "f" $ Abs "x" (iterate (Ref "f" @@) (Ref "x") !! n)

-- booleans
true  = Abs "t" $ Abs "f" $ Ref "t"
false = Abs "t" $ Abs "f" $ Ref "f"

-- Y combinator
ycomb = Abs "r" (e @@ e)
  where e = Abs "a" (Ref "r" @@ (Ref "a" @@ Ref "a"))


--
-- feature modeling functions
--

-- optionality
opt = Abs "f" $ Abs "base"
    $ Dim "Opt" ["n","y"]
    $ Chc "Opt" [id, Ref "f"] @@ Ref "base"

-- exclusive or
altN n = fAbsN n $ Dim "Alt" fs $ Chc "Alt" (map Ref fs)
  where fs = fsN n

alt2 = altN 2
alt3 = altN 3

-- inclusive or
orN n = fAbsN n $ dim $ chc
  where fss = tail (subsequences (fsN n))
        dim = Dim "Or" (map concat fss)
        chc = Chc "Or" $ map (foldr1 (\f g -> dot @@ f @@ g) . map Ref . reverse) fss

or2,or3 :: CC String
or2 = orN 2
or3 = orN 3

-- apply a feature an arbitrary number of times
arb :: CC String
arb = Abs "f" $ Abs "base" $ rec @@ rec
  where rec = Abs "r" $ Dim "Again?" ["n","y"]
                      $ Chc "Again?" [Ref "base", Ref "f" @@ (Ref "r" @@ Ref "r")]

-- n..m copies of the given feature (not exactly the standard usage)
ntom n m = Abs "f" $ Abs "base"
         $ Dim "NtoM" (map show nm)
         $ Chc "NtoM" [num i @@ Ref "f" | i <- nm] @@ Ref "base"
  where nm = [n..m]


--
-- helper functions
--

fsN :: Int -> [Var]
fsN n = map (('f':) . show) [1..n]

fAbsN :: Int -> CC a -> CC a
fAbsN n body = foldr ($) (Abs "base" (body @@ Ref "base")) (map Abs (fsN n))



--
-- examples
--

-- arbitrary data
a = leaf "a"
b = leaf "b"
c = leaf "c"

-- some really basic generic features
pre  = Abs "pre"  $ Abs "x" $ Str "" [Ref "pre", Ref "x"]
post = Abs "post" $ Abs "x" $ Str "" [Ref "x", Ref "post"]
wrap = Abs "pre"  $ Abs "post" $ Abs "x" $ Str "" [Ref "pre", Ref "x", Ref "post"]

-- some specific features
pre'  = pre  @@ a
post' = post @@ c
wrap' = wrap @@ a @@ c

-- basic application of features
ab  = pre'  @@ b
bc  = post' @@ b
abc = wrap' @@ b

-- applying a feature multiple times
pre4 = num 4 @@ pre' @@ bc

-- apply one feature (x)or another
preXORpost = alt2 @@ pre' @@ post' @@ b
preORpost  = or2  @@ pre' @@ post' @@ b

-- apply a feature once or twice
prePre = opt @@ (dot @@ pre' @@ (opt @@ pre')) @@ b

-- apply a feature an arbitrary number of times
preArb  = arb @@ pre' @@ b
preArb5 = pretty $ take 5 $ sem preArb -- print out the first five entries

-- apply a feature two to four times
pre24 = ntom 2 4 @@ pre' @@ b

-- choose two to four of any feature
any24 = ntom 2 4 @@ (alt2 @@ pre' @@ post') @@ b

-- a more complicated feature model
fm :: CC String
fm = opt @@ (alt3 @@ (dot @@ (opt @@ post') @@ pre')
                  @@ (dot @@ (opt @@ pre' ) @@ post')
                  @@ wrap') @@ b

--
-- demonstrations of "true" compositional approach
--

world = leaf "world"
hiBye = Dim "Say" ["hi","bye"] $ Chc "Say" [leaf "Hello ", leaf "Goodbye "]
hiBye' = Abs "x"
       $ Dim "Say" ["hi","bye"]
       $ Chc "Say" [leaf "Hello ", leaf "Goodbye "]
         @@ Ref "x"
         @@ Chc "Say" [leaf "!", leaf "..."]

count = Str 1 [leaf 2, leaf 3]
ext = Str 0 [leaf 0, leaf 0, leaf 4]
--inc = Str 1 [leaf 1, leaf 1]
--extInc = Dim "Do" ["ext","inc"] $ Chc "Do" [ext,inc]

type R = (Maybe Int,[CC Int])

merge :: Maybe R -> Maybe R -> Maybe R
merge (Just (Just a ,xs)) (Just (Just b ,ys)) | a == b = Just (Just a,  nub (xs++ys))
merge (Just (Just a ,xs)) (Just (Nothing,ys))          = Just (Just a,  nub (xs++ys))
merge (Just (Nothing,xs)) (Just (Just b ,ys))          = Just (Just b,  nub (xs++ys))
merge (Just (Nothing,xs)) (Just (Nothing,ys))          = Just (Nothing, nub (xs++ys))
merge _ _ = Nothing

inc :: Maybe R -> Maybe R
inc (Just (a,xs))  = Just (Just (maybe 1 (+1) a),xs)
inc _ = Nothing

dec :: Maybe R -> Maybe R
dec (Just (a,xs)) = Just (fmap (subtract 1) a,xs)
dec _ = Nothing

applT :: CC Int -> CC Int -> Bool
applT (Str a []) (Str b []) = a < b

compT :: CC Int -> CC Int -> CC Int
compT (Str a []) (Str b []) = Str (a+b) []

compA :: CC Int -> Maybe Int
compA (Str a _) = Just a

plain :: CC Int -> Bool
plain (Str _ es) = all plain es
plain _ = False

sat :: Map Var Int -> [Int] -> CC Int -> Maybe Int
sat _ _ e | plain e = compA e
sat m _ (Ref v) = lookup v m
sat m as (Str _ es)  = do ss <- sequence (map (sat m as) es)
                          if all (==0) ss then Just 0 else Nothing
sat m as (Dim _ _ e) = sat m as e
sat m as (Chc _ es)  = do (s:ss) <- sequence (map (sat m as) es) 
                          if all (==s) ss then Just s else Nothing
sat m []     (Abs v e) = fmap (+1) $ sat ((v,0):m) [] e
sat m (a:as) (Abs v e) = fmap (+1) $ sat ((v,a):m) as e
sat m as (App l r)   = do sr <- sat m [] r
                          sl <- sat m (sr:as) l
                          if sl > 0 then Just (sl-1) else Nothing


{-
sat :: Map Var R -> [R] -> CC Int -> Maybe R
sat _ _  (Str x [])  = Just (Nothing,[leaf x])
--sat o m as (Str x es)  = do ss <- sequence (map (sat o m as) es)
--                            if all (==0) ss then Just (0,x) else Nothing
sat m as (Dim _ _ e) = sat m as e
sat m as (Chc _ es)  = foldr1 merge (map (sat m as) es)
sat m []     (Abs v e) = inc $ sat ((v,(Nothing,[])):m) [] e
sat m (a:as) (Abs v e) = inc $ sat ((v,a):m) as e
sat m as (Ref v)     = lookup v m
sat m as (App l r)   = do (ar,xr) <- sat m []           r
                          (al,xl) <- sat m ((ar,xr):as) l
                          if maybe True (>0) al && and [applT x y | x <- xl, y <- xr]
                             then Just (fmap (subtract 1) al, [compT x y | x <- xl, y <- xr]) else Nothing
-}

check :: CC Int -> Maybe Int
check = sat [] []

{-
check :: CC Int -> Bool
check e = do (a,_) <- sat' e False
-}

