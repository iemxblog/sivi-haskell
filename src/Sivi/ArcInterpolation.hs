{-|
Module		: Sivi.ArcInterpolation
Description	: Interpolation of arcs
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.ArcInterpolation
(
	arcInterpolation
) where

import Linear
import Sivi.IR

arcInterpolation :: 	Double 			-- ^ ai : Angle increment
			-> Instruction 		-- ^ Instruction to interpolate
			-> [Instruction]
--arcInterpolation ai (Move dst (Arc dir cen f)) = 
--	where 
--		axis =

arcInterpolation ai i = [i] -- sort of identity function, so that the code compiles (before the real code gets implemented in a next commit)
