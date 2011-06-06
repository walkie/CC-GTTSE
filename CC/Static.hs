
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
boundVars e           = unions (ccQ boundVars e)

freeDims :: Data a => V a -> Set Dim
freeDims (Dim d _ e) = d `delete` freeDims e
freeDims (Chc d es)  = d `insert` unions (map freeDims es)
freeDims e           = unions (ccQ freeDims e)

freeVars :: Data a => V a -> Set Var
freeVars (Shr v b u) = freeVars b `union` (v `delete` freeVars u)
freeVars (Ref v)     = singleton v
freeVars e           = unions (ccQ freeVars e)

-- variable substitution
subVar :: Data a => Var -> V a -> V a -> V a
subVar v e   (Ref w)     | v == w = e
subVar v e f@(Shr w _ _) | v == w = f
subVar v e f = ccT (subVar v e) f


----------------
-- X-Freeness --
----------------

dimFree :: Data a => V a -> Bool
dimFree (Dim _ _ _) = False
dimFree e           = and (ccQ dimFree e)

chcFree :: Data a => V a -> Bool
chcFree (Chc _ _) = False
chcFree e         = and (ccQ chcFree e)

shrFree :: Data a => V a -> Bool
shrFree (Shr _ _ _) = False
shrFree e           = and (ccQ shrFree e)

refFree :: Data a => V a -> Bool
refFree (Ref _) = False
refFree e       = and (ccQ refFree e)

-- variation free
vFree :: Data a => V a -> Bool
vFree e = dimFree e && chcFree e

-- sharing free
sFree :: Data a => V a -> Bool
sFree e = shrFree e && chcFree e

plain :: Data a => V a -> Bool
plain e = vFree e && sFree e


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
wellFormed :: Data a => V a -> Bool
wellFormed e = wellDim [] e && wellRef e
