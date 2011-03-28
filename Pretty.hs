module Pretty (pretty,psem) where

import CC hiding (red)

import Data.List (intersperse)

---------------------
-- Pretty Printing --
---------------------

commas c = concat . intersperse (c ",")
parens s = op "(" ++ s ++ op ")"

op  = style blue
key = style (blue ++ bold)
var = style red
dim = style green
tag = style green

showStr :: String -> [String] -> String
showStr a [] = a
showStr a es = a ++ "{" ++ commas id es ++ "}"
  
showDim :: Dim -> [Tag] -> String -> String
showDim d ts e = key "dim " ++ dim d ++ op "<" ++ commas op (map tag ts) ++ op ">" ++
                 key " in " ++ e

showChc :: Dim -> [String] -> String
showChc d es = dim d ++ op "<" ++ commas op es ++ op ">"

instance Data a => Show (Value a) where
  show (Value e) = show e
  show (Closure m e) = env ++ ':' : show e
    where env = "[" ++ commas id (map entry m) ++ "]"
          entry (v,p) = "(" ++ var v ++ "," ++ show p ++ ")"

instance Show QTag where
  show (Q d t) = tag (d ++ "." ++ t)

instance Data a => Show (CC a) where
  show (Str a es)   = showStr (showData a) (map show es)
  show (Dim d ts e) = showDim d ts (show e)
  show (Chc d es)   = showChc d (map show es)
  show (Abs v e)    = op "\\" ++ var v ++ op ". " ++ show e
  show (App l r)    = parens (show l) ++ " " ++ parens (show r)
  show (Ref v)      = var v

pretty :: Data a => Semantics a -> IO ()
pretty = mapM_ putStrLn . map row
  where row (qs,e) = "[" ++ commas id (map show qs) ++ "]" ++ "  =>  " ++ show e

psem :: Data a => CC a -> IO ()
psem = pretty . sem


---------------------------
-- Pretty Printing Types --
---------------------------

instance Show t => Show (CCT t) where
  show (StrT t es)   = showStr (show t) (map show es)
  show (DimT d ts e) = showDim d ts (show e)
  show (ChcT d es)   = showChc d (map show es)
  show (FunT l r)    = parens (show l ++ " -> " ++ show r)
  show (VarT v)      = var (v2s v)
    where v2s v | v < 26    = ['a'..] !! v : []
                | otherwise = v2s (d-1) ++ v2s m where (d,m) = divMod v 26


-- Martin's color module (modified)
--

reset = "\27[0m"
bold  = "\27[1m"

attrFG c = "\27[3" ++ show c ++ "m"

black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

defaultColor = black ++ reset

style c s = c ++ s ++ defaultColor
