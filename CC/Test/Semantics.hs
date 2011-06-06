module CC.Test.Semantics where

import CC.Syntax
import CC.Semantics
import CC.Test.Framework

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
  [[da],[dadbShr]]
      -- da
  [ [ [([a],1),([b],2)] ],
      -- dadbShr
    [ [([a,c],1),
       ([a,d],3),
       ([b,c],2),
       ([b,d],3)] ] ]

-----------------
-- Expressions --
-----------------

-- tag shortcuts
a = Q "A" "a"
b = Q "A" "b"
c = Q "B" "c"
d = Q "B" "d"

da = dimA $ Chc "A" [Obj 1, Obj 2] :: V Int
db = dimB $ Chc "B" [Ref "x", Obj 3]
dadbShr = Shr "x" da db
