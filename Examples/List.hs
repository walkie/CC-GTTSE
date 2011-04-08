
module Examples.List where

import CC.Syntax
import CC.Semantics
import CC.List


-- variational foldr
vFoldr :: (a -> b -> b) -> b -> List a -> V b
vFoldr _ b Empty      = Obj b
vFoldr f b (Cons a l) = fmap (f a) (vFoldr f b l)
vFoldr f b (VList vl) = vl >>= vFoldr f b

-- [A<1,2>,3]
e1 :: List Int
e1 = VList $ Let "v" (Obj $ Cons 3 Empty)
           $ dimA $ Chc "A" [cons 1 (Ref "v"), cons 2 (Ref "v")]

-- [A<1,2>,3]
e2 :: List Int
e2 = VList $ App (dimA $ Chc "A" [Obj $ Cons 1 Empty, Obj $ Cons 2 Empty])
                 (Obj $ Cons 3 Empty)

{- from GHCi:

-- incorrect

Examples.List> vFoldr (+) 0 e1
let v = 3 in dim A<a,b> in A<v,v>

Examples.List> psem $ vFoldr (+) 0 e1 
[A.a]  =>  3
[A.b]  =>  3

-- correct

Examples.List> vFoldr (+) 0 e2
(dim A<a,b> in A<1,2>) (3)

Examples.List> psem $ vFoldr (+) 0 e2
[A.a]  =>  4
[A.b]  =>  5

-}
