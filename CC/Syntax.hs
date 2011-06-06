{-# LANGUAGE DeriveDataTypeable #-}

module CC.Syntax where

import Control.Applicative   (Applicative (pure,(<*>)))
import Control.Monad         (ap,liftM,liftM2)
import Control.Monad.State   (evalState,get,put)
import Data.Foldable         (Foldable (foldMap))
import Data.Functor          ((<$>))
import Data.Functor.Identity (Identity (Identity,runIdentity))
import Data.Generics         (Data,Typeable,cast,gmapQ,gmapM)
import Data.Monoid           (mappend,mconcat,mempty)
import Data.Traversable      (Traversable (sequenceA,traverse))

import CC.Pretty

------------
-- Syntax --
------------

type Name = String
type Dim = Name
type Tag = Name
type Var = Name

-- choice calculus expressions (read: variational x)
data V a =
    Obj a               -- object language stuff
  | Dim Dim [Tag] (V a) -- dimension declaration
  | Chc Dim [V a]       -- choice branching
  | Shr Var (V a) (V a) -- static variable binding
  | Ref Var             -- variable reference
  deriving (Eq,Data,Typeable)

-- true if the top node is of the corresponding syntactic category
isObj, isDim, isChc, isShr, isRef :: V a -> Bool
isObj (Obj _)     = True
isObj _           = False
isDim (Dim _ _ _) = True
isDim _           = False
isChc (Chc _ _)   = True
isChc _           = False
isShr (Shr _ _ _) = True
isShr _           = False
isRef (Ref _)     = True
isRef _           = False

class Data a => VT a where
  cleanup :: a -> a

instance VT Int  where cleanup = id
instance VT Bool where cleanup = id
instance VT Char where cleanup = id
instance VT a => VT [a] where
  cleanup = map cleanup


------------------------
-- Smart Constructors --
------------------------

-- a list of names derived from the given name, for example:
-- names 4 "x" == ["x1","x2","x3","x4"]
names :: Name -> Int -> [Name]
names x n = [x ++ show i | i <- [1..n]]

-- construct a dimension with arbitrary tag names
dimN :: Dim -> Int -> V a -> V a
dimN d n = Dim d (names "t" n)

-- some simple binary dimensions
dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d"]
dimC = Dim "C" ["e","f"]


---------------
-- SYB Stuff --
---------------

-- generic query on immediate CC subexpressions
-- in object language data types
gccQ :: (Typeable a, Data b) => (V a -> r) -> b -> [r]
gccQ f b = case cast b of
             Nothing -> concat (gmapQ (gccQ f) b)
             Just va -> [f va]

-- generic monadic transformation on immediate CC subexpressions
-- in object language data types
gccM :: (Monad m, Typeable a, Data b) => (V a -> m (V a)) -> b -> m b
gccM f b = case cast b of
             Nothing -> gmapM (gccM f) b 
             Just va -> liftM (maybe b id . cast) (f va)

-- generic transformation on immediate CC subexpressions
-- in object language data types
gccT :: (Typeable a, Data b) => (V a -> V a) -> b -> b
gccT f = runIdentity . gccM (Identity . f)


----------------------
-- Helper Functions --
----------------------

-- apply a query to all immediate CC subexpressions
ccQ :: Data a => (V a -> r) -> V a -> [r]
ccQ f (Obj a)     = gccQ f a
ccQ f (Dim _ _ e) = [f e]
ccQ f (Chc _ es)  = map f es
ccQ f (Shr _ b u) = [f b,f u]
ccQ f (Ref _)     = []

-- apply a monadic transformation to all CC subexpressions
ccM :: (Monad m, Data a) => (V a -> m (V a)) -> V a -> m (V a)
ccM f (Obj a)     = liftM Obj (gccM f a)
ccM f (Dim d t e) = liftM (Dim d t) (f e)
ccM f (Chc d es)  = liftM (Chc d) (mapM f es)
ccM f (Shr v b u) = liftM2 (Shr v) (f b) (f u)
ccM f (Ref v)     = return (Ref v)

-- apply a transformation to all CC subexpressions
ccT :: Data a => (V a -> V a) -> V a -> V a
ccT f = runIdentity . ccM (Identity . f)

-- immediate CC subexpressions
subs :: Data a => V a -> [V a]
subs = ccQ id

-- swap the immediate CC subexpressions
swap :: Data a => V a -> [V a] -> V a
swap = evalState . ccM (const swap')
  where swap' = do { (e:es) <- get; put es; return e }


---------------
-- Instances --
---------------

instance Monad V where
  return = Obj
  Obj a     >>= f = f a
  Dim d t e >>= f = Dim d t (e >>= f)
  Chc d es  >>= f = Chc d (map (>>= f) es)
  -- Shr v b u >>= f = Shr v (b >>= f) (u >>= f)
  -- Ref v     >>= _ = Ref v

instance Applicative V where
  pure  = return
  (<*>) = ap

instance Functor V where
  fmap f e = e >>= return . f

instance Foldable V where
  foldMap f (Obj a)     = f a
  foldMap f (Dim _ _ e) = foldMap f e
  foldMap f (Chc _ es)  = mconcat (map (foldMap f) es)
  foldMap f (Shr _ b u) = foldMap f b `mappend` foldMap f u
  foldMap f (Ref v)     = mempty
  
instance Traversable V where
  traverse f (Obj a)     = Obj     <$> f a
  traverse f (Dim d t e) = Dim d t <$> traverse f e
  traverse f (Chc d es)  = Chc d   <$> sequenceA (map (traverse f) es)
  traverse f (Shr v b u) = Shr v   <$> traverse f b <*> traverse f u
  traverse f (Ref v)     = pure (Ref v)

instance Show a => Show (V a) where
  show (Obj a)     = show a
  show (Dim d t e) = showDim d t (show e)
  show (Chc d es)  = showChc d (map show es)
  show (Shr v b u) = showShr v (show b) (show u)
  show (Ref v)     = showRef v
