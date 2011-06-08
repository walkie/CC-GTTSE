
module CC.Share where

import Data.Generics (Data)
import Data.Generics.Zipper
import Data.Set (member)

import CC.Syntax
import CC.List

import CC.Static
import CC.Semantics


-- this is wrong, since we could have dims and choices within Objs also...
share :: Data a => V a -> (V a -> V a) -> V a
share (Dim d ts e) f = Dim d ts $ share e f
share (Chc d es)   f = Chc d [share e f | e <- es]
share e f | vFree e   = f e
          | otherwise = share (helper 0 e) f

helper :: Data a => Int -> V a -> V a
helper i e | i < length ss = case s of
    Dim d ts e' -> Dim d ts (helper i (swap e (h++e':t)))
    Chc d es    -> Chc d    [helper i (swap e (h++e':t)) | e' <- es]
    _           -> helper (i+1) (swap e (h ++ helper 0 s : t))
           | otherwise = e
  where ss       = subs e
        (h,s:t)  = splitAt i ss



e1  = Shr "v" ab1 $ Ref "v"
e1' = share   ab1 (\v -> v)

-- e2  = Shr "v" ab1 $ vcat (Ref "v") (Ref "v")
e2' = share e1' (\v -> vcat v v)

add1 = fmap (fmap (+1)) e2'
