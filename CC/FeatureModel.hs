module CC.FeatureModel where

import Data.List (subsequences)

import CC.Syntax
import CC.LambdaCalculus

--
-- feature modeling functions
--

-- optionality
opt :: V a
opt = Abs "f" $ base
    $ Dim "Opt" ["n","y"]
    $ Chc "Opt" [ccid, Ref "f"]

-- exclusive or
altN :: Int -> V a
altN n = abss fs $ base $ Dim "Alt" fs $ Chc "Alt" (map Ref fs)
  where fs = names "f" n

alt2,alt3 :: V a
alt2 = altN 2
alt3 = altN 3

-- inclusive or
orN :: Int -> V a
orN n = abss fs $ base $ dim $ chc
  where fs  = names "f" n
        fss = tail (subsequences fs)
        dim = Dim "Or" (map concat fss)
        chc = Chc "Or" $ map (foldr1 (\f g -> dot @@ f @@ g) . map Ref . reverse) fss

or2,or3 :: V a
or2 = orN 2
or3 = orN 3

-- apply a feature an arbitrary number of times
arb :: V a
arb = Abs "f" $ Abs "b" $ rec @@ rec
  where rec = Abs "r" $ Dim "Again?" ["n","y"]
                      $ Chc "Again?" [Ref "b", Ref "f" @@ (Ref "r" @@ Ref "r")]

-- n..m copies of the given feature (not exactly the standard usage)
ntom :: Int -> Int -> V a
ntom n m = Abs "f" $ base
         $ Dim "NtoM" (map show nm)
         $ Chc "NtoM" [num i @@ Ref "f" | i <- nm]
  where nm = [n..m]

-- (helper function) abstraction capturing and applying the base program
base e = Abs "b" (e @@ Ref "b")
