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
  deriving (Eq,Data,Typeable)

-- true if the top node is of the corresponding syntactic category
isObj, isDim, isChc :: V a -> Bool
isObj (Obj _)     = True
isObj _           = False
isDim (Dim _ _ _) = True
isDim _           = False
isChc (Chc _ _)   = True
isChc _           = False

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
-- 
dimN :: Dim -> Int -> V a -> V a
dimN d n = Dim d (names "" n)

-- injecting plain values into V
--
--  NOTE: maybe call it "lift" ??
--
plain :: a -> V a
plain = Obj

chc = Chc

chc' :: Dim -> [a] -> V a
chc' d = Chc d . map plain

-- atomic dimension/choice
aDim  d ts cs = Dim d ts $ chc d cs
aDim' d ts cs = Dim d ts $ chc' d cs


-- lifting a function's argument type to V
liftV :: (a -> V b) -> V a -> V b
liftV = (=<<)
--liftV = flip (>>=)
--liftV f = (>>= f)


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

-- apply a monadic transformation to all CC subexpressions
ccM :: (Monad m, Data a) => (V a -> m (V a)) -> V a -> m (V a)
ccM f (Obj a)     = liftM Obj (gccM f a)
ccM f (Dim d t e) = liftM (Dim d t) (f e)
ccM f (Chc d es)  = liftM (Chc d) (mapM f es)

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

-- Functor is simpler than, and "comes before",  Monad.
-- We should be able to understand the Functor instance independently of
-- the Monad instance (which might not even exist!)
--
--   fmap f e = e >>= return . f
--
instance Functor V where
  fmap f (Obj o)      = Obj (f o)
  fmap f (Dim d ts e) = Dim d ts (fmap f e)
  fmap f (Chc d es)   = Chc d (map (fmap f) es)
  
instance Monad V where
  return = Obj
  Obj a     >>= f = f a
  Dim d t e >>= f = Dim d t (e >>= f)
  Chc d es  >>= f = Chc d (map (>>= f) es)

instance Applicative V where
  pure  = return
  (<*>) = ap

instance Foldable V where
  foldMap f (Obj a)     = f a
  foldMap f (Dim _ _ e) = foldMap f e
  foldMap f (Chc _ es)  = mconcat (map (foldMap f) es)
  
instance Traversable V where
  traverse f (Obj a)     = Obj     <$> f a
  traverse f (Dim d t e) = Dim d t <$> traverse f e
  traverse f (Chc d es)  = Chc d   <$> sequenceA (map (traverse f) es)

instance Show a => Show (V a) where
  show (Obj a)     = show a
  show (Dim d t e) = showDim d t (show e)
  show (Chc d es)  = showChc d (map show es)
