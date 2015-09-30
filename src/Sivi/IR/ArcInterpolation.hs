{-|
Module		: Sivi.IR.ArcInterpolation
Description	: Interpolation of arcs
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.IR.ArcInterpolation
(
	arcInterpolation
) where

import Linear
import Sivi.IR.Base
import Sivi.IR.PositionTracking
import Sivi.Range

-- | Function that interpolates a single 'Sivi.IR.Base.Instruction'
arcInterpolation' :: 	Double 			-- ^ ai : Angle increment (in degrees)
			-> Instruction 		-- ^ Instruction to interpolate
			-> TrackedPosition [Instruction]
arcInterpolation' ai (Move dst (Arc dir cen f)) = do
		cp <- getPosition
		let a = cp
		let b = dst
		let o = cen
		let oa = a - o
		let ob = b - o
		let axis = cross oa ob
		let alpha = atan2 (norm (cross oa ob)) (dot oa ob)
		let alpha' = case dir of
			CW -> -2 * pi - alpha
			CCW -> alpha
		let step = case dir of
			CW -> ai
			CCW -> negate ai
		let angles = range 0 alpha' step
		return [Move (o + rotate (axisAngle axis angle) oa) (LinearInterpolation f) | angle <- angles]
arcInterpolation' _ i = return [i]

-- | Interpolates arcs (transforms them into a list of linear interpolations)
arcInterpolation :: Double		-- ^ ai : Angle increment (in degrees)
		-> [Instruction]	-- ^ List of instructions
		-> [Instruction]
arcInterpolation ai = concat . mapTrackPosition (arcInterpolation' ai)
