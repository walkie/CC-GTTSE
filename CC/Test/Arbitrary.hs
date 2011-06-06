{-# LANGUAGE FlexibleInstances #-}

--
-- A module for generating random, well-formed choice calculus expressions.
--
module CC.Test.Arbitrary where

import Control.Monad (liftM,liftM2)
import Test.QuickCheck.Gen
import Test.QuickCheck

import CC.Syntax


-----------------------
-- Simple Generators --
-----------------------

-- generate a dimension name
genDimName :: Gen Dim
genDimName = liftM (:[]) (elements ['A'..'C'])

-- generate a tag name
genTagName :: Gen Tag
genTagName = liftM (:[]) (elements ['a'..'e'])

-- generate a list of tags
genTagNames :: Gen [Tag]
genTagNames = listOf genTagName

-- generate a variable name
genVarName :: Gen Var
genVarName = genTagName


--------------------------------
-- Choice Calculus Generators --
--------------------------------

type GenV a = GenState a -> Gen (V a)

data GenState a = GenState {
  genData   :: GenState a -> Gen a,
  maxDepth  :: Int,
  maxBranch :: Int,
  dimEnv    :: [(Dim,Int)],
  varEnv    :: [Var]
}

--
-- state manipulation functions

initGenState :: (GenState a -> Gen a) -> Int -> Int -> GenState a
initGenState g d b = GenState g d b [] []

incDepth :: GenState a -> GenState a
incDepth s = s { maxDepth = maxDepth s - 1 }

addDim :: Dim -> [Tag] -> GenState a -> GenState a
addDim d ts s = s { dimEnv = (d, length ts) : filter ((d/=) . fst) (dimEnv s) }

addVar :: Var -> GenState a -> GenState a
addVar v s = s { varEnv = v : varEnv s }

delVar :: Var -> GenState a -> GenState a
delVar v s = s { varEnv = filter (/=v) (varEnv s) }

--
-- generators

genCC :: GenV a
genCC s | maxDepth s < 2 = genObj (incDepth s)
        | otherwise      = oneof (map ($ incDepth s) gens)
  where gens = concat
             $ zipWith replicate 
               [1, 1, 4, 4, 2]  -- generator frequencies
             $ [genObj, genShr, genDim]
               ++ if null (dimEnv s) then [] else [genChc]
               ++ if null (varEnv s) then [] else [genRef]

genObj :: GenV a
genObj s = liftM Obj (genData s s)

genDim :: GenV a
genDim s = do d  <- genDimName
              t1 <- genTagName
              t2 <- genTagName `suchThat` (/=t1)
              let ts = [t1,t2]
              -- ts <- resize (maxBranch s - 2) genTagNames
              e  <- genCC (addDim d ts s)
              return (Dim d ts e)

genChc :: GenV a
genChc s = do (d,i) <- elements (dimEnv s)
              as    <- vectorOf i (genCC s)
              return (Chc d as)

genShr :: GenV a
genShr s = do v <- genVarName
              b <- genCC s
              u <- genCC (addVar v s)
              return (Shr v b u)

genRef :: GenV a
genRef s = liftM Ref (elements (varEnv s))


------------------------
-- Arbitrary Instance --
------------------------

arbitraryCC = genCC (initGenState (\_ -> elements [0..99]) 10 3)

instance Arbitrary (V Int) where
  arbitrary = arbitraryCC
