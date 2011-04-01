
module CC.Static where

import Data.Generics (Data)
import Data.Set (Set,delete,fromList,insert,null,singleton,union,unions)
import Prelude hiding (null)

import CC.Syntax

-----------
-- Types --
-----------

type Map k v = [(k,v)]


------------------------------
-- Dimensions and Variables --
------------------------------

dimDecls :: Data a => V a -> Map Dim [Tag]
dimDecls (Dim d t e) = (d,t) : dimDecls e
dimDecls e           = concat (ccQ dimDecls e)

boundDims :: Data a => V a -> Set Dim
boundDims = fromList . map fst . dimDecls

boundTags :: Data a => V a -> Set Tag
boundTags = fromList . concat . map snd . dimDecls

boundVars :: Data a => V a -> Set Var
boundVars (Shr v b u) = v `insert` boundVars b `union` boundVars u
boundVars (Let v b u) = v `insert` boundVars b `union` boundVars u
boundVars (Abs v e)   = v `insert` boundVars e
boundVars e           = unions (ccQ boundVars e)

freeDims :: Data a => V a -> Set Dim
freeDims (Dim d _ e) = d `delete` freeDims e
freeDims (Chc d es)  = d `insert` unions (map freeDims es)
freeDims e           = unions (ccQ freeDims e)

freeVars :: Data a => V a -> Set Var
freeVars (Shr v b u) = freeVars b `union` (v `delete` freeVars u)
freeVars (Let v b u) = v `delete` (freeVars b `union` freeVars u)
freeVars (Abs v e)   = v `delete` freeVars e
freeVars (Ref v)     = singleton v
freeVars e           = unions (ccQ freeVars e)


---------------------
-- Well Formedness --
---------------------

-- well dimensioned
wellDim :: Data a => Map Dim Int -> V a -> Bool
wellDim m (Dim d ts e) = wellDim ((d,length ts):m) e
wellDim m (Chc d es)   = maybe False (== length es) (lookup d m) && all (wellDim m) es
wellDim m e            = and $ ccQ (wellDim m) e

-- well referenced
wellRef :: Data a => V a -> Bool
wellRef = null . freeVars

-- well formed
well :: Data a => V a -> Bool
well e = wellDim [] e && wellRef e
