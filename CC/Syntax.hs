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

-- Choice calculus expressions (read: variational x)
data V a =
    Obj a               -- object language stuff
  | Dim Dim [Tag] (V a) -- dimension declaration
  | Chc Dim [V a]       -- choice branching
  deriving (Eq,Data,Typeable)


------------------------
-- Smart Constructors --
------------------------

-- a list of names derived from the given name, for example:
-- names 4 "x" == ["x1","x2","x3","x4"]
names :: Name -> Int -> [Name]
names x n = [x ++ show i | i <- [1..n]]

-- construct a dimension with arbitrary tag names
dimN :: Dim -> Int -> V a -> V a
dimN d n = Dim d (names "" n)

-- function names for choice and object constructors
chc = Chc
obj = Obj

chc' :: Dim -> [a] -> V a
chc' d = Chc d . map Obj

-- atomic dimension/choice
atomic  d ts cs = Dim d ts $ chc d cs
atomic' d ts cs = Dim d ts $ chc' d cs

-- tagging variational data
type Tagged a = (Tag,V a)

infixl 2 <:

(<:) :: Tag -> V a -> Tagged a
t <: v = (t,v)

-- constructing alternatives from tagged data
alt :: Dim -> [Tagged a] -> V a
alt d tvs = atomic d ts vs where (ts,vs) = unzip tvs

-- lifting a function's argument type to V
liftV :: (a -> V b) -> V a -> V b
liftV = (=<<)


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
  Obj a      >>= f = f a
  Dim d ts e >>= f = Dim d ts (e >>= f)
  Chc d es   >>= f = Chc d (map (>>= f) es)

instance Applicative V where
  pure  = return
  (<*>) = ap

instance Foldable V where
  foldMap f (Obj a)     = f a
  foldMap f (Dim _ _ e) = foldMap f e
  foldMap f (Chc _ es)  = mconcat (map (foldMap f) es)
  
instance Traversable V where
  traverse f (Obj a)      = Obj      <$> f a
  traverse f (Dim d ts e) = Dim d ts <$> traverse f e
  traverse f (Chc d es)   = Chc d    <$> sequenceA (map (traverse f) es)

instance Show a => Show (V a) where
  show (Obj a)      = show a
  show (Dim d ts e) = showDim d ts (show e)
  show (Chc d es)   = showChc d (map show es)
