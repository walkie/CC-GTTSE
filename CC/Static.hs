
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

freeDims :: Data a => V a -> Set Dim
freeDims (Dim d _ e) = d `delete` freeDims e
freeDims (Chc d es)  = d `insert` unions (map freeDims es)
freeDims e           = unions (ccQ freeDims e)

dimFree :: Data a => V a -> Bool
dimFree (Dim _ _ _) = False
dimFree e           = and (ccQ dimFree e)

chcFree :: Data a => V a -> Bool
chcFree (Chc _ _) = False
chcFree e         = and (ccQ chcFree e)

-- variation free
vFree :: Data a => V a -> Bool
vFree e = dimFree e && chcFree e


---------------------
-- Well Formedness --
---------------------

-- well dimensioned
wellDim :: Data a => Map Dim Int -> V a -> Bool
wellDim m (Dim d ts e) = wellDim ((d,length ts):m) e
wellDim m (Chc d es)   = maybe False (== length es) (lookup d m) && all (wellDim m) es
wellDim m e            = and $ ccQ (wellDim m) e
