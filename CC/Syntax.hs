{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

module CC.Syntax where

import Control.Applicative   (Applicative (pure,(<*>)))
import Control.Monad         (ap,liftM,liftM2)
import Control.Monad.State   (State,evalState,get,put)
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

type Dim  = String
type Tag  = String
type Var  = String

-- choice calculus expressions (read: variational x)
data V a =
    Obj a               -- object language stuff
  | Dim Dim [Tag] (V a) -- dimension declaration
  | Chc Dim [V a]       -- choice branching
  | Shr Var (V a) (V a) -- static sharing abstraction
  | Let Var (V a) (V a) -- recursive let abstraction
  | Abs Var (V a)       -- lambda abstraction
  | App (V a) (V a)     -- application
  | Ref Var             -- variable reference
  deriving (Eq, Data, Typeable)

class Compose a where
  -- Object language composition.
  -- The implementation of this method should assume that the only
  -- choice calculus constructor encountered will be Obj.
  (<.>) :: a -> a -> a


------------------------
-- Smart Constructors --
------------------------

(@@) :: V a -> V a -> V a
(@@) = App
infixl 1 @@

-- construct a dimension with arbitrary tag names
dimN :: Dim -> Int -> V a -> V a
dimN d n = Dim d ['t' : show i | i <- [1..n]]

dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d"]
dimC = Dim "C" ["e","f"]


-- some useful lambda calculus functions
--

-- identity
ccid = Abs "x" (Ref "x")

-- booleans
cctrue  = Abs "t" $ Abs "f" $ Ref "t"
ccfalse = Abs "t" $ Abs "f" $ Ref "f"

-- function composition
ccdot = Abs "f" $ Abs "g" $ Abs "x"
      $ Ref "f" @@ (Ref "g" @@ Ref "x")

-- church numerals
ccnum n = Abs "f" $ Abs "x" (iterate (Ref "f" @@) (Ref "x") !! n)

-- fixpoint combinator (Y)
ccfix = Abs "r" (e @@ e)
  where e = Abs "a" (Ref "r" @@ (Ref "a" @@ Ref "a"))


----------------------
-- Helper functions --
----------------------

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

-- apply a query to all immediate CC subexpressions
ccQ :: Data a => (V a -> r) -> V a -> [r]
ccQ f (Obj a)     = gccQ f a
ccQ f (Dim _ _ e) = [f e]
ccQ f (Chc _ es)  = map f es
ccQ f (Shr _ b u) = [f b,f u]
ccQ f (Let _ b u) = [f b,f u]
ccQ f (Abs _ e)   = [f e]
ccQ f (App l r)   = [f l,f r]
ccQ f (Ref _)     = []

-- apply a monadic transformation to all CC subexpressions
ccM :: (Monad m, Data a) => (V a -> m (V a)) -> V a -> m (V a)
ccM f (Obj a)     = liftM   Obj (gccM f a)
ccM f (Dim d t e) = liftM  (Dim d t) (f e)
ccM f (Chc d es)  = liftM  (Chc d) (mapM f es)
ccM f (Shr v b u) = liftM2 (Shr v) (f b) (f u)
ccM f (Let v b u) = liftM2 (Let v) (f b) (f u)
ccM f (Abs v e)   = liftM  (Abs v) (f e)
ccM f (App l r)   = liftM2  App (f l) (f r)
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

inObj :: Data a => (V a -> V a) -> a -> a
inObj f a = case f (Obj a) of
              Obj a -> a
              _ -> error "inObj"


---------------
-- Instances --
---------------

instance Compose (V a) where
  (<.>) = App

instance Monad V where
  
  return = Obj
  
  Obj a     >>= f = f a
  Dim d t e >>= f = Dim d t (e >>= f)
  Chc d es  >>= f = Chc d (map (>>= f) es)
  Shr v b u >>= f = Shr v (b >>= f) (u >>= f)
  Let v b u >>= f = Let v (b >>= f) (u >>= f)
  Abs v e   >>= f = Abs v (e >>= f)
  App l r   >>= f = App (l >>= f) (r >>= f)
  Ref v     >>= _ = Ref v

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
  foldMap f (Let _ b u) = foldMap f b `mappend` foldMap f u
  foldMap f (Abs _ e)   = foldMap f e
  foldMap f (App l r)   = foldMap f l `mappend` foldMap f r
  foldMap f (Ref v)     = mempty
  
instance Traversable V where
  traverse f (Obj a)     = Obj     <$> f a
  traverse f (Dim d t e) = Dim d t <$> traverse f e
  traverse f (Chc d es)  = Chc d   <$> sequenceA (map (traverse f) es)
  traverse f (Shr v b u) = Shr v   <$> traverse f b <*> traverse f u
  traverse f (Let v b u) = Let v   <$> traverse f b <*> traverse f u
  traverse f (Abs v e)   = Abs v   <$> traverse f e
  traverse f (App l r)   = App     <$> traverse f l <*> traverse f r
  traverse f (Ref v)     = pure (Ref v)

instance Show a => Show (V a) where
  show (Obj a)     = show a
  show (Dim d t e) = showDim d t (show e)
  show (Chc d es)  = showChc d (map show es)
  show (Shr v b u) = showShr v (show b) (show u)
  show (Let v b u) = showLet v (show b) (show u)
  show (Abs v e)   = showAbs v (show e)
  show (App l r)   = showApp (show l) (show r)
  show (Ref v)     = showRef v


--
-- Some trivial Compose instances for testing
--

instance Compose Int where
  (<.>) = (+)

instance Compose String where
  (<.>) = (++)
