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

-- | Function that interpolates an arc. Circles are possible only in the XY plane (quick fix), because it is not possible to know in which plane they are (the 3 points used to define them are colinear). Arcs are possible in 3D if the starting, ending and center are not colinear.
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
		let axisTmp = cross oa ob	-- temporary fix for circles...
		let axis = case axisTmp of
			V3 0 0 0 -> V3 0 0 1
			_ -> axisTmp
		let alpha = atan2 (norm (cross oa ob)) (dot oa ob)
		let alpha' = case dir of
			CW -> -2 * pi - alpha
			CCW -> case alpha of
					0 -> 2*pi
					_ -> alpha
		let step = case dir of
			CW -> (negate ai) * pi / 180
			CCW -> ai * pi / 180
		let angles = range 0 alpha' step
		return [Move (o + rotate (axisAngle axis angle) oa) (LinearInterpolation f) | angle <- angles]
arcInterpolation' _ i = return [i]

-- | Interpolates arcs (transforms them into a list of linear interpolations)
arcInterpolation :: Double		-- ^ ai : Angle increment (in degrees)
		-> [Instruction]	-- ^ List of instructions
		-> [Instruction]
arcInterpolation ai = concat . mapTrackPosition (arcInterpolation' ai)
