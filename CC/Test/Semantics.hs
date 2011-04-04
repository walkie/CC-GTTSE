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
  [[da],[dadbAbs,dadbLet,dadbShr]]
      -- da
  [ [ [([a],1),([b],3)] ],
      -- dadbAbs
    [ [([c,a],1),
       ([c,b],3),
       ([d,a,a],2),
       ([d,a,b],4),
       ([d,b,a],4),
       ([d,b,b],6)],
      -- dadbLet
      [([c,a],1),
       ([c,b],3),
       ([d,a,a],2),
       ([d,a,b],4),
       ([d,b,a],4),
       ([d,b,b],6)],
      -- dadbShr
      [([a,c],1),
       ([a,d],2),
       ([b,c],3),
       ([b,d],6)] ] ]

-----------------
-- Expressions --
-----------------

-- tag shortcuts
a = Q "A" "a"
b = Q "A" "b"
c = Q "B" "c"
d = Q "B" "d"

da = dimA $ Chc "A" [Obj 1, Obj 3] :: V Int
db = dimB $ Chc "B" [Ref "x", Ref "x" @@ Ref "x"]
dadbAbs = Abs "x" db @@ da
dadbLet = Let "x" da db
dadbShr = Shr "x" da db
