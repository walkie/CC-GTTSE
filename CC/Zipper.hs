{-# LANGUAGE Rank2Types #-}

module CC.Zipper where

import Control.Monad ((>=>))
import Data.Maybe    (isNothing)

import Data.Generics
import Data.Generics.Zipper -- requires, from Hackage: syz

import CC.Syntax


---------------
-- CC Zipper --
---------------

type Z a = Zipper (V a)


-------------
-- Queries --
-------------

-- True if the top node is of the corresponding syntactic category.
isCC, isObj, isDim, isChc :: V a -> Bool
isCC  _           = True
isObj (Obj _)     = True
isObj _           = False
isDim (Dim _ _ _) = True
isDim _           = False
isChc (Chc _ _)   = True
isChc _           = False

-- Are we at a location that satisfies the query?
atX :: Typeable a => (V a -> Bool) -> Z a -> Bool
atX f = query (mkQ False f)

-- Are we at a node of the corresponding syntactic category.
atCC, atObj, atDim, atChc :: Typeable a => Z a -> Bool
atCC  = atX isCC
atObj = atX isObj
atDim = atX isDim
atChc = atX isChc

-- Are we at the top/bottom/leftEnd/rightEnd of the expression?
atTop, atBottom, atLeftEnd, atRightEnd :: Typeable a => Z a -> Bool
atTop      = isNothing . up
atBottom   = isNothing . down
atLeftEnd  = isNothing . left
atRightEnd = isNothing . right


-----------
-- Moves --
-----------

type Move a = Z a -> Maybe (Z a)

-- Execute the given move if the given test passes.
moveIf :: (Z a -> Bool) -> Move a -> Move a
moveIf test move z | test z    = move z
                   | otherwise = Nothing

-- Move into a subexpression, dimension declaration, let-binding, or let-use.
inObj, inDim :: Typeable a => Move a
inObj  = moveIf atObj down
inDim  = moveIf atDim down

-- Move into a particular (indexed) alternative.
inAlt :: Typeable a => Int -> Move a
inAlt i = moveIf atChc $ foldl (>=>) down' (replicate (i+1) right)

-- Find the next matching location using a preorder traversal.
match :: Typeable a => (V a -> Bool) -> Move a
match f z | atX f z   = Just z
          | otherwise = case down' z >>= match f of
                          Nothing -> tryRight
                          success -> success
  where tryRight | atRightEnd z = Nothing
                 | otherwise    = right z >>= match f

---------------------
-- Transformations --
---------------------

type Trans  a = Z a -> Z a
type TransM a = Z a -> Maybe (Z a)

ccTrans :: Typeable a => (V a -> V a) -> Trans a
ccTrans f = trans (mkT f)

-- Apply a transformation to all matching nodes using a pre-order traversal.
transAll :: Typeable a => (Z a -> Bool) -> (Z a -> Z a) -> Z a -> Z a
transAll f t z | f z       = continue (t z)
               | otherwise = continue z
  where continue = rightT (transAll f t) . downT (transAll f t)

-- Cleanup a plain object.
clean :: Data a => a -> a
clean a = (unObj . fromZipper . cln . toZipper . Obj) a
  where cln z | downQ False atObj z = maybe err cln
                                    $ do Obj b <- (down z >>= getHole)
                                         return (setHole (b `asTypeOf` a) z)
              | otherwise = leftT cln (downT cln z)
        unObj (Obj a) = a
        err = error "clean: either expression is not plain or data type is malformed"

-- Create a new dimension at the current location.
newDim :: Typeable a => Dim -> [Tag] -> Trans a
newDim d ts = ccTrans (Dim d ts)

-- Create a new choice at the current location.
newChc :: Typeable a => Dim -> Int -> Trans a
newChc d n = ccTrans (Chc d . replicate n)
