module CC.LambdaCalculus where

import CC.Syntax

--
-- some useful lambda calculus functions
--

-- identity
ccid :: V a
ccid = Abs "x" (Ref "x")

-- booleans
true,false :: V a
true  = Abs "t" $ Abs "f" $ Ref "t"
false = Abs "t" $ Abs "f" $ Ref "f"

-- function composition
dot :: V a
dot = Abs "f" $ Abs "g" $ Abs "x"
    $ Ref "f" @@ (Ref "g" @@ Ref "x")

-- church numerals
num :: Int -> V a
num n = Abs "f" $ Abs "x" (iterate (Ref "f" @@) (Ref "x") !! n)

-- fixpoint combinator (Y)
fix :: V a
fix = Abs "r" (e @@ e)
  where e = Abs "a" (Ref "r" @@ (Ref "a" @@ Ref "a"))
