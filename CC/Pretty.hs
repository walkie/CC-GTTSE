
module CC.Pretty (commas,parens,bracks,braces,square,
                  showDim,showChc,showShr,showLet,
                  showAbs,showApp,showRef,
                  showSel,showEnv,showSem) where

import Data.List (intersperse)


---------------------
-- Pretty Printing --
---------------------

commas c = concat . intersperse (c ",")
parens s = op "(" ++ s ++ op ")"
bracks s = op "<" ++ s ++ op ">"
braces s = "{" ++ s ++ "}"
square s = "[" ++ s ++ "]"

declIn k decl body = key k ++ " " ++ decl ++ key " in " ++ body

op  = style blue
key = style (blue ++ bold)
var = style red
dim = style green
tag = style green

showDim d ts = declIn "dim" (dim d ++ bracks (commas op (map tag ts)))
showChc d es = dim d ++ bracks (commas op es)
showShr v b = declIn "share" (var v ++ op " = " ++ b)
showLet v b = declIn "let"   (var v ++ op " = " ++ b)
showAbs v e = op "\\" ++ var v ++ op ". " ++ e
showApp l r = parens l ++ " " ++ parens r
showRef v   = var v

showSel d t = tag (d ++ "." ++ t)

showEnv m = square (commas id (map entry m))
  where entry (v,p) = parens (var v ++ "," ++ show p)

showSem :: (Show a, Show b) => [([a],b)] -> String
showSem = concatMap row
  where row (qs,e) = square (commas id (map show qs)) ++ "  =>  " ++ show e ++ "\n"

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
