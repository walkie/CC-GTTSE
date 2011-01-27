
module Examples where

import CC
import Data.List (subsequences)
import Prelude hiding (id,and,or)


--
-- useful lambda calculus functions
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
opt = Abs "f" $ Abs "b"
    $ Dim "Opt" ["y","n"]
    $ Chc "Opt" [Ref "f", id] @@ Ref "b"

-- mutually exclusive
alt2 = Abs "f1" $ Abs "f2" $ Abs "b"
     $ Dim "Alt" ["f1","f2"]
     $ Chc "Alt" [Ref "f1", Ref "f2"] @@ Ref "b"

alt3 = Abs "f1" $ Abs "f2" $ Abs "f3" $ Abs "b"
     $ Dim "Alt" ["f1","f2","f3"]
     $ Chc "Alt" [Ref "f1", Ref "f2", Ref "f3"] @@ Ref "b"

or2 = Abs "f1" $ Abs "f2" $ Abs "b"
    $ Dim "Or" ["f1","f2","f1f2"]
    $ Chc "Or" [Ref "f1", Ref "f2", dot @@ Ref "f2" @@ Ref "f1"] @@ Ref "b"

or3 = Abs "f1" $ Abs "f2" $ Abs "f3" $ Abs "b"
    $ Dim "Or" ["f1","f2","f1f2","f3","f1f3","f2f3","f1f2f3"]
    $ Chc "Or" [Ref "f1", Ref "f2",
                dot @@ Ref "f2" @@ Ref "f1",
                Ref "f3",
                dot @@ Ref "f3" @@ Ref "f1",
                dot @@ Ref "f3" @@ Ref "f2",
                dot @@ Ref "f3" @@ (dot @@ Ref "f2" @@ Ref "f1")]
    @@ Ref "b"

orN n = abs $ Abs "b" $ dim $ chc @@ Ref "b"
  where fs  = map (('f':) . show) [1..n]
        fss = tail (subsequences fs)
        abs b = foldr ($) b (map Abs fs)
        dim = Dim "Or" (map concat fss)
        chc = Chc "Or" $ map (foldr1 (\f g -> dot @@ f @@ g) . map Ref . reverse) fss

-- apply a feature an arbitrary number of times
{-
arb = Abs "f" $ Abs "b" $ body
  where body = Dim "Again?" ["n","y"] $ Chc "Again?" [Ref "b", Ref "f" @@ body]
-}

arb = Abs "f" $ Abs "b" $ rec @@ rec
  where rec = Abs "r" $ Dim "Again?" ["n","y"]
                      $ Chc "Again?" [Ref "b", Ref "f" @@ (Ref "r" @@ Ref "r")]


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
