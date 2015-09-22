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
	backlashCompensation
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

backlash2 :: IR -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> IR
backlash2 [] _ _ _ _ = []
backlash2 (Move _ (Arc{}) : _) _ _ _ _ = error "Backlash compensation not implemented for arcs."
backlash2 (Move pos mp : xs) backlash ppos pdir pcomp = 
	if extraMove == V3 0 0 0 then Move compensatedPos mp : backlash2 xs backlash pos newdir compensation
	else Move extraMove mp : Move compensatedPos mp : backlash2 xs backlash pos newdir compensation
	where 
		sgn = signum (pos-ppos)
		newdir = onV3 mem pdir sgn
		dirChange = onV3 dirc pdir newdir
		compensation = pcomp + backlash * dirChange
		compensatedPos = pos + compensation
		extraMove = if dirChange == V3 0 0 0 then V3 0 0 0 else ppos + compensation
backlash2 (x : xs) backlash ppos pdir pcomp = x : backlash2 xs backlash ppos pdir pcomp

backlashCompensation :: IR		-- ^ The program to modifiy
			-> [V3 Double]  -- ^ The positions to prepend to the program to put the machine in a known backlash
			-> V3 Double 	-- ^ The backlash measured on the machine
			-> IR		-- ^ The modified program
backlashCompensation p initPos backlash = backlash2 (map (\x -> Move x Rapid) initPos ++ p) backlash (head initPos) (V3 0 0 0) (V3 0 0 0)

