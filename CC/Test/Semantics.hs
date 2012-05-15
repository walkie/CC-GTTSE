module CC.Test.Semantics where

import CC.Syntax
import CC.Semantics
import CC.Test.Framework

import Examples.Names

-----------------------
-- "Automated" Tests --
-----------------------

tests = test_sem
     ++ []

runTests = defaultMain tests


----------------
-- Test Cases --
----------------

test_sem = testSames "sem" sem
  ["basic","sharing"]
  [[da],[db]]
      -- da
  [ [ [([a],1),([b],2)] ],
      -- db
    [ [([c,a],1),
       ([c,b],2),
       ([d],3)] ] ]

-----------------
-- Expressions --
-----------------

-- tag shortcuts
a = Q "A" "a"
b = Q "A" "a'"
c = Q "B" "b"
d = Q "B" "b'"

da = dimA $ Chc "A" [Obj 1, Obj 2] :: V Int
db = dimB $ Chc "B" [da, Obj 3]
