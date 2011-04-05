
module Examples where

import CC.Syntax
import CC.Static
import CC.Semantics
import CC.Pretty
import CC.LambdaCalculus
import CC.FeatureModel

-- some really basic generic features
pre  = Abs "pre"  $ Abs "x" $ Ref "pre" @@ Ref "x"
post = Abs "post" $ Abs "x" $ Ref "x"   @@ Ref "post"
wrap = Abs "pre"  $ Abs "post" $ Abs "x" $ Ref "pre" @@ Ref "x" @@ Ref "post"

-- arbitrary data
a = Obj "a"
b = Obj "b"
c = Obj "c"

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
fm = opt @@ (alt3 @@ (dot @@ (opt @@ post') @@ pre')
                  @@ (dot @@ (opt @@ pre' ) @@ post')
                  @@ wrap') @@ b
