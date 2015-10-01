{-|
Module		: Sivi.Backlash
Description	: Modifies a program to compensate the backlash of the machine
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Backlash
(
	backlash
) where

import Linear
import Sivi.IR

-- | Memorization of direction.
mem :: (Eq a, Num a) => a -> a -> a
mem a 0 = a
mem 0 b = b
mem _ b = b

-- | Direction change.
dirc :: (Eq a, Num a) => a -> a -> a
dirc (-1) 1 = 1
dirc 1 (-1) = -1
dirc _ _ = 0


-- | Applies a function on pairs of coordinates of two V3 vectors.
onV3 :: (a -> a -> a) 	-- ^ f : Function to apply
	-> V3 a 	-- ^ First vector
	-> V3 a 	-- ^ Second vector
	-> V3 a
onV3 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')

backlash' :: IR -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> IR
backlash' [] _ _ _ _ = []
backlash' (Move _ (Arc{}) : _) _ _ _ _ = error "Backlash compensation not implemented for arcs. Use arcInterpolation before."
backlash' (Move pos mp : xs) backlashValues ppos pdir pcomp = 
	if extraMove == V3 0 0 0 then Move compensatedPos mp : backlash' xs backlashValues pos newdir compensation
	else Move extraMove mp' : Move compensatedPos mp : backlash' xs backlashValues pos newdir compensation
	where 
		sgn = signum (pos-ppos)
		newdir = onV3 mem pdir sgn
		dirChange = onV3 dirc pdir newdir
		compensation = pcomp + backlashValues * dirChange
		compensatedPos = pos + compensation
		extraMove = if dirChange == V3 0 0 0 then V3 0 0 0 else ppos + compensation
		mp' = case mp of
			Rapid -> Rapid
			LinearInterpolation f -> LinearInterpolation f
			Arc d c f -> Arc d c f
			Probe f -> LinearInterpolation f
backlash' (DefCurPos pos : xs) backlashValues _ pdir pcomp = DefCurPos (pos + pcomp) : backlash' xs backlashValues pos pdir pcomp
backlash' (x : xs) backlashValues ppos pdir pcomp = x : backlash' xs backlashValues ppos pdir pcomp

-- | Modifies a program to compensate the backlash of the machine.
backlash :: V3 Double 	-- ^ Position where to make backlash initialization (to put the machine in a known backlash)
			-> V3 Double 	-- ^ Backlash measured on the machine
			-> IR		-- ^ Program to modify
			-> IR		-- ^ Modified program
backlash initPos backlashValues p = backlash' (initMoves ++ p) backlashValues initPos (V3 (-1) (-1) (-1)) (V3 0 0 0)
	where initMoves = [Move (V3 0 0 0 + initPos) Rapid, Move (V3 1 1 1 + initPos) Rapid]
