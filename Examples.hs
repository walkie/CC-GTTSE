
module Examples where

import CC
import Data.List (subsequences)
import Prelude hiding (id,and,or)


--
-- useful pure lambda calculus functions
--

-- identity (duh)
id  = Abs "x" (Ref "x")

-- function composition
dot = Abs "f" $ Abs "g" $ Abs "x"
    $ Ref "f" @@ (Ref "g" @@ Ref "x")

-- church numerals
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

or2 = orN 2
or3 = orN 3

-- apply a feature an arbitrary number of times
arb = Abs "f" $ Abs "base" $ rec @@ rec
  where rec = Abs "r" $ Dim "Again?" ["n","y"]
                      $ Chc "Again?" [Ref "base", Ref "f" @@ (Ref "r" @@ Ref "r")]


--
-- helper functions
--

fsN :: Int -> [Var]
fsN n = map (('f':) . show) [1..n]

fAbsN :: Int -> CC -> CC
fAbsN n body = foldr ($) (Abs "base" (body @@ Ref "base")) (map Abs (fsN n))

--
-- example features
--

-- arbitrary base program
foo = leaf "foo"

-- generic wrapper
wrap = Abs "pre" $ Abs "post" $ Abs "x"
     $ Str ";" [Ref "pre", Ref "x", Ref "post"]

-- a specific wrapper
wrap' = wrap @@ leaf "before" @@ leaf "after"

-- a program with an optional wrapper
optWrap = opt @@ wrap' @@ foo

-- applying a wrapper four times
fourWrap = num 4 @@ wrap' @@ foo

-- wrap once or twice
oneOrTwo = opt @@ (dot @@ wrap' @@ (opt @@ wrap'))
