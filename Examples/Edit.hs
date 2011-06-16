
module Examples.Edit where

import Control.Monad ((>=>),msum)
import Data.Generics (Data,mkQ)
import Data.Generics.Zipper -- requires, from Hackage: syz
import Data.Maybe (fromJust)
import qualified Data.Set as S (member)

import CC.Syntax
import CC.Static
import CC.Zipper

--import CC.Semantics
import Examples.List
import Examples.Names


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


-- Hoist a dimension to the top level of an expression, indicating whether
-- or not choices were captured.
hoist :: Data a => Dim -> V a -> Maybe (V a, Bool)
hoist d e = do
    (ts, e') <- matchQueryEdit named tags strip e
    return (Dim d ts e', S.member d (freeDims e))
  where named (Dim d' _ _) = d == d'
        named _            = False
        tags  (Dim _ ts _) = ts
        strip (Dim _ _  e) = e


-- Simpler example of querying to get the tags associated with a dimension.
getTags :: Data a => Dim -> V a -> Maybe [Tag]
getTags d (Dim d' ts e) | d == d' = Just ts
getTags d e = msum (ccQ (getTags d) e)
