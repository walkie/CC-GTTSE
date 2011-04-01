{-# LANGUAGE FlexibleInstances #-}

--
-- A module for generating random, well-formed choice calculus expressions.
--
module Test where

import Control.Monad (liftM,liftM2)
import Test.QuickCheck.Gen
import Test.QuickCheck

import CC


-----------------------
-- Simple Generators --
-----------------------

-- generate a random data string
genData :: Gen Int
genData = elements [0..99]

-- generate a dimension name
genDimName :: Gen Dim
genDimName = liftM (:[]) (elements ['A'..'C'])

-- generate a tag name
genTagName :: Gen Tag
genTagName = liftM (:[]) (elements ['a'..'e'])

-- generate a variable name
genVarName :: Gen Var
genVarName = genTagName

-- generate a list of tags
genTagNames :: Gen [Tag]
genTagNames = listOf genTagName

tagGen :: [Tag]
tagGen = [ts++[t] | ts <- []:tagGen, t <- ['a'..'z']]
  
dimGen :: [Dim]
dimGen = [ds++[d] | ds <- []:dimGen, d <- ['A'..'Z']]


------------------------
-- CC Generator State --
------------------------

data GenState = GenState {
  maxDepth  :: Int,
  maxBranch :: Int,
  dimEnv    :: [(Dim,Int)],
  varEnv    :: [Var]
}

genState :: Int -> Int -> GenState
genState d b = GenState d b [] []

inc :: GenState -> GenState
inc s = s { maxDepth = maxDepth s - 1 }

addDim :: Dim -> [Tag] -> GenState -> GenState
addDim d ts s = s { dimEnv = (d, length ts) : filter ((d/=) . fst) (dimEnv s) }

addVar :: Var -> GenState -> GenState
addVar v s = s { varEnv = v : varEnv s }


--------------------------------
-- Choice Calculus Generators --
--------------------------------

type GenCC = GenState -> Gen (CC Int)

genCC :: GenCC
genCC s | maxDepth s < 2 = genLeaf (inc s)
        | otherwise      = oneof (map ($ inc s) gens)
  where gens = [genLeaf, genStr, genLet, genDim] 
            ++ if null (dimEnv s) then [] else [genChc]
            ++ if null (varEnv s) then [] else [genRef]

genLeaf :: GenCC
genLeaf s = liftM leaf genData

genStr :: GenCC
genStr s = do a  <- genData
              es <- resize (maxBranch s) (listOf (genCC s))
              return (Str a es)

genAbs :: GenCC
genAbs s = do v <- genVarName
              u <- genCC (addVar v s)
              return (Abs v u)

genApp :: GenCC
genApp s = liftM2 App (genCC s) (genCC s)

genLet :: GenCC
genLet s = do l <- genAbs s
              r <- genCC s
              return (App l r)

genRef :: GenCC
genRef s = liftM Ref (elements (varEnv s))

genDim :: GenCC
genDim s = do d  <- genDimName
              t  <- genTagName
              ts <- resize (maxBranch s - 1) genTagNames
              e  <- genCC (addDim d (t:ts) s)
              return (Dim d (t:ts) e)

genChc :: GenCC
genChc s = do (d,i) <- elements (dimEnv s)
              as    <- vectorOf i (genCC s)
              return (Chc d as)

------------------------
-- Arbitrary Instance --
------------------------

instance Arbitrary (CC Int) where
  arbitrary = genCC (genState 10 2)
