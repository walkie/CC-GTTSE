
module Examples.Edit where

import Control.Monad ((>=>),msum)
import Data.Generics (Data,mkQ,mkT)
import Data.Generics.Zipper -- requires, from Hackage: syz
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Set as Set (member)

import CC.Syntax
import CC.Static
import CC.Zipper

import CC.Semantics
import Examples.List
import Examples.Names


-----------------
-- Refactoring --
-----------------

type C a = Z a

type Locator a = V a -> Maybe (C a)
type Splitter a = V a -> Maybe (C a,V a)
-- type GSplitter a b = V a -> Maybe (C a,b)
-- type VSplitter a = Splitter a (V a)
-- type Splitters a = V a -> Maybe (C a,[V a])
type Trans a = V a -> V a
type Pred a = V a -> Bool


withFallback :: a -> Maybe a -> a
withFallback = fromMaybe

runEdit :: Data a => (C a -> C a) -> V a -> V a
runEdit f = fromZipper . f . toZipper

find ::  Data a => Pred a -> Locator a
-- find ::  Data a => (V a -> Bool) -> Locator a
find p = match p . toZipper

-- extract :: Data a => (V a -> Bool) -> Splitter a (V a)
extract :: Data a => Pred a -> Splitter a
-- extract :: (Data a,Data b) => (V a -> Bool) -> GSplitter a b
extract p e = do
    c <- find p e
    h <- getHole c
    return (c,h)

apply :: Data a => C a -> V a -> V a
apply c h = fromZipper (setHole h c)

infixr <@
(<@) :: Data a => C a -> V a -> V a
(<@) = apply


-- extractDim :: Data a => Dim -> VSplitter a
-- extractDim d = extract (dimDef d)

dimDef :: Dim -> Pred a
dimDef d (Dim d' _ _) = d == d'
dimDef _ _            = False

chcFor :: Dim -> Pred a
chcFor d (Chc d' _)  = d == d'
chcFor _ _           = False


-- hoist :: Data a => Dim -> V a -> Maybe (V a)
-- hoist d e = do
hoist :: Data a => Dim -> V a -> V a
hoist d e = withFallback e $ do
    (c,Dim _ ts e') <- extract (dimDef d) e
    return (Dim d ts (c <@ e'))

-- safeHoist :: Data a => Dim -> V a -> Maybe (V a)
-- safeHoist d e = do
safeHoist :: Data a => Dim -> V a -> V a
safeHoist d e = withFallback e $ do
    (c,Dim _ ts e') <- extract (dimDef d) e
    if d `Set.member` freeDims e then
       Nothing
    else
       return (Dim d ts (c <@ e'))

-- assumes priorized choice sits in 2nd alternative
prioritize :: Data a => Dim -> Dim -> V a -> V a
prioritize b a e = withFallback e $ do
    (dA,ae)            <- extract (dimDef a) e
    (cA,Chc _ [a1,a2]) <- extract (chcFor a) ae
    (cB,Chc _ [b1,b2]) <- extract (chcFor b) a2
    return $ dA <@ (Chc b [cB <@ b1,cA <@ (Chc a [a1,cB <@ b2])])

-- Apply a transformation to all matching nodes
-- stopping recursion on the end condition.
editBetween :: Data a => (V a -> Bool) -> (V a -> Bool) -> (V a -> V a) -> C a -> C a
editBetween b e t c | atX b c = continue (trans (mkT t) c)
                    | atX e c = leftT edit c
                    | otherwise = continue c
  where continue = leftT edit . downT edit
        edit = editBetween b e t

-- inRange :: Data a => (V a -> V a) -> (V a -> Bool,V a -> Bool) -> V a -> V a
inRange :: Data a => (V a -> V a) -> (Pred a,Pred a) -> V a -> V a
inRange f (begin,end) e = runEdit (editBetween begin end f) e

-- given an n-ary choice, produce an (n+1)-ary choice where the ith alternative
-- has been copied and appended to the end. i is indexed from 1.
copyAlt :: Int -> V a -> V a
copyAlt i (Chc d as) = Chc d (as ++ [as !! (i-1)])

-- add a new alternative to the end of a choice
addAlt :: V a -> V a -> V a
addAlt a (Chc d as) = Chc d (as ++ [a])

-- extend a dimension with a new tag, transforming all bound choices
extend :: Data a => Dim -> Tag -> (V a -> V a) -> V a -> V a
extend d t f e = withFallback e $ do
    (c, Dim _ ts e) <- extract (dimDef d) e
    let e' = runEdit (editBetween (chcFor d) (dimDef d) f) e
    return (c <@ Dim d (ts++[t]) e')


extend' :: Data a => Dim -> Tag -> (V a -> V a) -> V a -> V a
extend' d t f e = withFallback e $ do
    (c, Dim _ ts e) <- extract (dimDef d) e
    let e' = f `inRange` (chcFor d,dimDef d) $ e
    return (c <@ Dim d (ts++[t]) e')



{-

e = dA < ...
a2 = cB < ...

dA < cA < A[a1,a2 : cB < B[b1,b2]] 

-->

dA < B[cB < b1, cA < A[a1,cB < b2]]

-}

invert :: Data a => Dim -> Dim -> V a -> V a
invert b a = prioritize b a . hoist b

-- Hoist a dimension to the top level of an expression, indicating whether
-- or not choices were captured.
-- xhoist :: Data a => Dim -> V a -> Maybe (V a, Bool)
-- xhoist d e = do (ts, e') <- matchQueryEdit named tags strip e
--                 return (Dim d ts e', Set.member d (freeDims e))
--   where named (Dim d' _ _) = d == d'
--         named _            = False
--         tags  (Dim _ ts _) = ts
--         strip (Dim _ _  e) = e


-- Simpler example of querying to get the tags associated with a dimension.
getTags :: Data a => Dim -> V a -> Maybe [Tag]
getTags d (Dim d' ts e) | d == d' = Just ts
getTags d e = msum (ccQ (getTags d) e)


--
-- Examples, move!
--

menuD :: VList Food
menuD = chc "Dessert" [vsingle Cake,
            atomic "Main" ["meat","pasta"] 
             [vlist [Steak,Fries],Pasta `cons` dessert]]


dessert' = opt "Dessert" Cake
meat     = "meat"  <: vlist [Steak,Fries]
pasta    = "pasta" <: Pasta `cons` dessert'
menu'    = alt "Main" [meat,pasta]



bs = [vsingle "b1",vsingle "b2"]

ba :: VList String
ba = dimB $ dimA $ chcA [vsingle "a1","a2" `cons` chcB bs]

b_a :: VList String
b_a = dimB $ chcB [vlist ["a2","b1"], dimA $ chcA [vsingle "a1",vlist ["a2","b2"]]]

b_a' = prioritize "B" "A" ba
test = b_a' == b_a

a_b :: VList String
a_b = dimA $ chcA [vsingle "a1", "a2" `cons` dimchcB bs]


dMenu = hoist "Dessert" menu

dMenu' = prioritize "Dessert" "Main" dMenu

