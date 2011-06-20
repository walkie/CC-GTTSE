
module Examples.Edit where

import Control.Monad ((>=>),msum)
import Data.Generics (Data,mkQ)
import Data.Generics.Zipper -- requires, from Hackage: syz
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Set as Set (member)

import CC.Syntax
import CC.Static
import CC.Zipper

--import CC.Semantics
import Examples.List
import Examples.Names


--------------
-- Building --
--------------

opt :: Dim -> a -> VList a
opt n x = aDim n ["yes","no"] [vsingle x,vempty]

type Tagged a = (Tag,V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t,v)

alt :: Dim -> [Tagged a] -> V a
alt n tvs = aDim n ts vs where (ts,vs) = unzip tvs


-----------------
-- Refactoring --
-----------------

-- helper function
matchQueryEdit :: Data a =>
                  (V a -> Bool)  -- predicate to match on
               -> (V a -> r)     -- query to perform on matched value
               -> (V a -> V a)   -- transformation to apply to matched value
               -> V a            -- initial expression
               -> Maybe (r, V a) -- result of query and modified expression
matchQueryEdit p q t e = do 
    z <- match p (toZipper e)
    m <- getHole z
    return (q m, fromZipper (setHole (t m) z))


type C a = Z a

type Locator a = V a -> Maybe (C a)
type GSplitter a b = V a -> Maybe (C a,b)
-- type VSplitter a = Splitter a (V a)
type Splitter a = V a -> Maybe (C a,V a)
type Splitters a = V a -> Maybe (C a,[V a])


withFallback :: a -> Maybe a -> a
withFallback = fromMaybe


find ::  Data a => (V a -> Bool) -> Locator a
find p = match p . toZipper

-- extract :: Data a => (V a -> Bool) -> Splitter a (V a)
extract :: Data a => (V a -> Bool) -> Splitter a
-- extract :: (Data a,Data b) => (V a -> Bool) -> GSplitter a b
extract p e = do
    c <- find p e
    h <- getHole c
    return (c,h)

apply :: Data a => C a -> V a -> V a
apply c h = fromZipper (setHole h c)


-- extractDim :: Data a => Dim -> VSplitter a
-- extractDim d = extract (dimDef d)

dimDef :: Dim -> V a -> Bool
dimDef d (Dim d' _ _) = d == d'
dimDef _ _            = False

chcIn :: Dim -> V a -> Bool
chcIn d (Chc d' _)  = d == d'
chcIn _ _           = False


-- hoist :: Data a => Dim -> V a -> Maybe (V a)
-- hoist d e = do
hoist :: Data a => Dim -> V a -> V a
hoist d e = withFallback e $ do
    (c,Dim _ ts e') <- extract (dimDef d) e
    return (Dim d ts (apply c e'))

-- safeHoist :: Data a => Dim -> V a -> Maybe (V a)
-- safeHoist d e = do
safeHoist :: Data a => Dim -> V a -> V a
safeHoist d e = withFallback e $ do
    (c,Dim _ ts e') <- extract (dimDef d) e
    if d `Set.member` freeDims e then
       Nothing
    else
       return (Dim d ts (apply c e'))

-- swapOpt :: Data a => Dim -> Dim -> Int -> V a -> V a
-- swapOpt o a k e = withFallback e $ do
--     (cDa,eDa)         <- extract (dimDef d) e
--     (cCa,Chc _ eas)   <- extract (chcIn a) eDa
--     (cCo,Chc _ [y,_]) <- extract (chcIn o) (eas !! (k-1))
--     return $ apply cDa (Chc o [apply cCo y,apply cCa])
--     
--     apply c' (apply c vempty))
-- 
-- {-
-- 
-- cDa[ cCa[ cCo[y,_]  ] ] 
-- -->
-- cDa[ O[ cCo[y], cCa[] ] ] 
-- 
-- -}


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
            aDim "Main" ["meat","pasta"] 
             [vlist [Steak,Fries],Pasta `cons` dessert]]


dessert' = opt "Dessert" Cake
meat     = "meat"  <: vlist [Steak,Fries]
pasta    = "pasta" <: Pasta `cons` dessert'
menu'    = alt "Main" [meat,pasta]



dimO = Dim "O" ["yes","no"]
chcO y = chc "O" [vsingle y,vempty]



a_o :: VList String
a_o = dimA $ chcA [vsingle "a", "b" `cons` dimO (chcO "y")]

oa :: VList String
oa = dimO $ dimA $ chcA [vsingle "a","b" `cons` chcO "y"]

o_a :: VList String
o_a = dimO $ chc "O" [vlist ["b","y"], dimA $ chcA [vsingle "a",vsingle "b"]]
