{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

module CC.Robot where

import Control.Monad (liftM,liftM2,liftM3)
import Data.Generics (Data,Typeable)

import CC.Syntax
import CC.List

import CC.Static
import CC.Semantics


-- instructions
--

data Inst = TurnL | TurnR | Inc | Dec | NoOp
  deriving (Eq,Show,Data,Typeable)


-- direction
--

data Dir = N | E | S | W
  deriving (Eq,Bounded,Enum,Show,Data,Typeable)

left :: Dir -> Dir
left N = W
left d = pred d

right :: Dir -> Dir
right W = N
right d = succ d


-- robot state
--

type Speed = Int

data Robot = Robot Dir Speed
  deriving (Eq,Show,Data,Typeable)

init = Robot N 0

-- execute one instruction on a robot
exec :: Robot -> Inst -> Robot
exec (Robot d s) TurnL = Robot (left  d) s
exec (Robot d s) TurnR = Robot (right d) s
exec (Robot d s) Inc   = Robot d (s+1)
exec (Robot d s) Dec   = Robot d (s-1)
exec r           NoOp  = r

-- execute a sequence of instructions, returning the final robot state
execAll :: Robot -> List Inst -> Robot
execAll r = foldl exec r . fromList


-- world state
--

type Pos = (Int,Int)

start = (0,0)

-- run one step of the simulation given a robot state and its position
step :: Pos -> Robot -> Pos
step (x,y) (Robot N s) = (x  ,y+s)
step (x,y) (Robot E s) = (x+s,y  )
step (x,y) (Robot S s) = (x  ,y-s)
step (x,y) (Robot W s) = (x-s,y  )

-- run the simulation given an initial position and robot state and a list
-- of instructions to execute
run :: Pos -> Robot -> List Inst -> Pos
run p r = foldl step p . scanl exec r . fromList


-- variational versions of the above functions
--

vexec :: V Robot -> V Inst -> V Robot
vexec = liftM2 exec

vexec' :: V Robot -> Inst -> V Robot
vexec' r = vexec r . Obj

vexecAll :: V Robot -> V (List Inst) -> V Robot
vexecAll = vfoldl vexec'

vstep :: V Pos -> V Robot -> V Pos
vstep = liftM2 step

vstep' :: V Pos -> Robot -> V Pos
vstep' p = vstep p . Obj

vrun :: V Pos -> V Robot -> V (List Inst) -> V Pos
vrun p r = vfoldl vstep' p . vscanl vexec' r


-- instances
--

instance VT Inst  where cleanup = id
instance VT Robot where cleanup = id
instance VT Pos   where cleanup = id


-- examples
--

s :: V Pos
s = dimA $ Chc "A" [Obj (0,0), Obj (5,5)]

r :: V Robot
r = dimB $ Chc "B" [Obj (Robot N 1), Obj (Robot E 2)]

i :: V Inst
i = dimC $ Chc "C" [Obj TurnL, Obj TurnR]

is :: V (List Inst)
is = NoOp `vcons` (dimC $ Chc "C" [TurnL `vcons` Inc `vcons` tail, Dec `vcons` tail])
  where tail = NoOp `vcons` vempty

{- in GHCi:

*CC.Robot> s
dim A<a,b> in A<(0,0),(5,5)>

*CC.Robot> r
dim B<c,d> in B<Robot N 1,Robot E 2>

*CC.Robot> i
dim C<e,f> in C<TurnL,TurnR>

*CC.Robot> is
NoOp:dim C<e,f> in C<TurnL:Inc:NoOp:[],Dec:NoOp:[]>

*CC.Robot> psem $ vexec r i
[B.c,C.e]  =>  Robot W 1
[B.c,C.f]  =>  Robot E 1
[B.d,C.e]  =>  Robot N 2
[B.d,C.f]  =>  Robot S 2

*CC.Robot> psem $ vexecAll r is
[C.e,B.c]  =>  Robot W 2
[C.e,B.d]  =>  Robot N 3
[C.f,B.c]  =>  Robot N 0
[C.f,B.d]  =>  Robot E 1

*CC.Robot> psem $ vstep s r
[A.a,B.c]  =>  (0,1)
[A.a,B.d]  =>  (2,0)
[A.b,B.c]  =>  (5,6)
[A.b,B.d]  =>  (7,5)

*CC.Robot> psem $ vrun s r is
[B.c,B.c,C.e,B.c,B.c,B.c,A.a]  =>  (-5,2)
[B.c,B.c,C.e,B.c,B.c,B.c,A.b]  =>  (0,7)
[B.c,B.c,C.e,B.c,B.c,B.d,A.a]  =>  (-3,5)
[B.c,B.c,C.e,B.c,B.c,B.d,A.b]  =>  (2,10)
[B.c,B.c,C.e,B.c,B.d,B.c,A.a]  =>  (-3,5)
[B.c,B.c,C.e,B.c,B.d,B.c,A.b]  =>  (2,10)
[B.c,B.c,C.e,B.c,B.d,B.d,A.a]  =>  (-1,8)
[B.c,B.c,C.e,B.c,B.d,B.d,A.b]  =>  (4,13)
[B.c,B.c,C.e,B.d,B.c,B.c,A.a]  =>  (-4,4)
[B.c,B.c,C.e,B.d,B.c,B.c,A.b]  =>  (1,9)
[B.c,B.c,C.e,B.d,B.c,B.d,A.a]  =>  (-2,7)
[B.c,B.c,C.e,B.d,B.c,B.d,A.b]  =>  (3,12)
[B.c,B.c,C.e,B.d,B.d,B.c,A.a]  =>  (-2,7)
[B.c,B.c,C.e,B.d,B.d,B.c,A.b]  =>  (3,12)
[B.c,B.c,C.e,B.d,B.d,B.d,A.a]  =>  (0,10)
[B.c,B.c,C.e,B.d,B.d,B.d,A.b]  =>  (5,15)
[B.c,B.c,C.f,B.c,B.c,A.a]  =>  (0,2)
[B.c,B.c,C.f,B.c,B.c,A.b]  =>  (5,7)
[B.c,B.c,C.f,B.c,B.d,A.a]  =>  (1,2)
[B.c,B.c,C.f,B.c,B.d,A.b]  =>  (6,7)
[B.c,B.c,C.f,B.d,B.c,A.a]  =>  (1,2)
[B.c,B.c,C.f,B.d,B.c,A.b]  =>  (6,7)
[B.c,B.c,C.f,B.d,B.d,A.a]  =>  (2,2)
[B.c,B.c,C.f,B.d,B.d,A.b]  =>  (7,7)
[B.c,B.d,C.e,B.c,B.c,B.c,A.a]  =>  (-3,1)
[B.c,B.d,C.e,B.c,B.c,B.c,A.b]  =>  (2,6)
[B.c,B.d,C.e,B.c,B.c,B.d,A.a]  =>  (-1,4)
[B.c,B.d,C.e,B.c,B.c,B.d,A.b]  =>  (4,9)
[B.c,B.d,C.e,B.c,B.d,B.c,A.a]  =>  (-1,4)
[B.c,B.d,C.e,B.c,B.d,B.c,A.b]  =>  (4,9)
[B.c,B.d,C.e,B.c,B.d,B.d,A.a]  =>  (1,7)
[B.c,B.d,C.e,B.c,B.d,B.d,A.b]  =>  (6,12)
[B.c,B.d,C.e,B.d,B.c,B.c,A.a]  =>  (-2,3)
[B.c,B.d,C.e,B.d,B.c,B.c,A.b]  =>  (3,8)
[B.c,B.d,C.e,B.d,B.c,B.d,A.a]  =>  (0,6)
[B.c,B.d,C.e,B.d,B.c,B.d,A.b]  =>  (5,11)
[B.c,B.d,C.e,B.d,B.d,B.c,A.a]  =>  (0,6)
[B.c,B.d,C.e,B.d,B.d,B.c,A.b]  =>  (5,11)
[B.c,B.d,C.e,B.d,B.d,B.d,A.a]  =>  (2,9)
[B.c,B.d,C.e,B.d,B.d,B.d,A.b]  =>  (7,14)
[B.c,B.d,C.f,B.c,B.c,A.a]  =>  (2,1)
[B.c,B.d,C.f,B.c,B.c,A.b]  =>  (7,6)
[B.c,B.d,C.f,B.c,B.d,A.a]  =>  (3,1)
[B.c,B.d,C.f,B.c,B.d,A.b]  =>  (8,6)
[B.c,B.d,C.f,B.d,B.c,A.a]  =>  (3,1)
[B.c,B.d,C.f,B.d,B.c,A.b]  =>  (8,6)
[B.c,B.d,C.f,B.d,B.d,A.a]  =>  (4,1)
[B.c,B.d,C.f,B.d,B.d,A.b]  =>  (9,6)
[B.d,B.c,C.e,B.c,B.c,B.c,A.a]  =>  (-3,1)
[B.d,B.c,C.e,B.c,B.c,B.c,A.b]  =>  (2,6)
[B.d,B.c,C.e,B.c,B.c,B.d,A.a]  =>  (-1,4)
[B.d,B.c,C.e,B.c,B.c,B.d,A.b]  =>  (4,9)
[B.d,B.c,C.e,B.c,B.d,B.c,A.a]  =>  (-1,4)
[B.d,B.c,C.e,B.c,B.d,B.c,A.b]  =>  (4,9)
[B.d,B.c,C.e,B.c,B.d,B.d,A.a]  =>  (1,7)
[B.d,B.c,C.e,B.c,B.d,B.d,A.b]  =>  (6,12)
[B.d,B.c,C.e,B.d,B.c,B.c,A.a]  =>  (-2,3)
[B.d,B.c,C.e,B.d,B.c,B.c,A.b]  =>  (3,8)
[B.d,B.c,C.e,B.d,B.c,B.d,A.a]  =>  (0,6)
[B.d,B.c,C.e,B.d,B.c,B.d,A.b]  =>  (5,11)
[B.d,B.c,C.e,B.d,B.d,B.c,A.a]  =>  (0,6)
[B.d,B.c,C.e,B.d,B.d,B.c,A.b]  =>  (5,11)
[B.d,B.c,C.e,B.d,B.d,B.d,A.a]  =>  (2,9)
[B.d,B.c,C.e,B.d,B.d,B.d,A.b]  =>  (7,14)
[B.d,B.c,C.f,B.c,B.c,A.a]  =>  (2,1)
[B.d,B.c,C.f,B.c,B.c,A.b]  =>  (7,6)
[B.d,B.c,C.f,B.c,B.d,A.a]  =>  (3,1)
[B.d,B.c,C.f,B.c,B.d,A.b]  =>  (8,6)
[B.d,B.c,C.f,B.d,B.c,A.a]  =>  (3,1)
[B.d,B.c,C.f,B.d,B.c,A.b]  =>  (8,6)
[B.d,B.c,C.f,B.d,B.d,A.a]  =>  (4,1)
[B.d,B.c,C.f,B.d,B.d,A.b]  =>  (9,6)
[B.d,B.d,C.e,B.c,B.c,B.c,A.a]  =>  (-1,0)
[B.d,B.d,C.e,B.c,B.c,B.c,A.b]  =>  (4,5)
[B.d,B.d,C.e,B.c,B.c,B.d,A.a]  =>  (1,3)
[B.d,B.d,C.e,B.c,B.c,B.d,A.b]  =>  (6,8)
[B.d,B.d,C.e,B.c,B.d,B.c,A.a]  =>  (1,3)
[B.d,B.d,C.e,B.c,B.d,B.c,A.b]  =>  (6,8)
[B.d,B.d,C.e,B.c,B.d,B.d,A.a]  =>  (3,6)
[B.d,B.d,C.e,B.c,B.d,B.d,A.b]  =>  (8,11)
[B.d,B.d,C.e,B.d,B.c,B.c,A.a]  =>  (0,2)
[B.d,B.d,C.e,B.d,B.c,B.c,A.b]  =>  (5,7)
[B.d,B.d,C.e,B.d,B.c,B.d,A.a]  =>  (2,5)
[B.d,B.d,C.e,B.d,B.c,B.d,A.b]  =>  (7,10)
[B.d,B.d,C.e,B.d,B.d,B.c,A.a]  =>  (2,5)
[B.d,B.d,C.e,B.d,B.d,B.c,A.b]  =>  (7,10)
[B.d,B.d,C.e,B.d,B.d,B.d,A.a]  =>  (4,8)
[B.d,B.d,C.e,B.d,B.d,B.d,A.b]  =>  (9,13)
[B.d,B.d,C.f,B.c,B.c,A.a]  =>  (4,0)
[B.d,B.d,C.f,B.c,B.c,A.b]  =>  (9,5)
[B.d,B.d,C.f,B.c,B.d,A.a]  =>  (5,0)
[B.d,B.d,C.f,B.c,B.d,A.b]  =>  (10,5)
[B.d,B.d,C.f,B.d,B.c,A.a]  =>  (5,0)
[B.d,B.d,C.f,B.d,B.c,A.b]  =>  (10,5)
[B.d,B.d,C.f,B.d,B.d,A.a]  =>  (6,0)
[B.d,B.d,C.f,B.d,B.d,A.b]  =>  (11,5)

-}
