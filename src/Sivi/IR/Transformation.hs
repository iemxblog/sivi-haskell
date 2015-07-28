{-|
Module		: Sivi.IR.Transformation
Description	: Transformations (rotations, etc) of intermediate representation
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.IR.Transformation
(
	rotateIR
) where

import Linear
import Sivi.IR.Base

-- | Applies a matrix transformation to move parameters.
applyMatrixMP :: M33 Double 	-- ^ m : The transformation matrix
		-> MoveParams	-- ^ The move parameters
		-> MoveParams	-- ^ The transformed move parameters
applyMatrixMP _ Rapid = Rapid
applyMatrixMP _ (LinearInterpolation f) = LinearInterpolation f
applyMatrixMP m (Arc dir center f) = Arc dir (m !* center) f

-- | This function applies a matrix transformation to an instruction (of Intermediate Representation)
applyMatrix' :: M33 Double		-- ^ m : The transformation matrix	
		-> Instruction		-- ^ p : The program in Intermediate Representation
		-> Instruction		-- ^ The resulting Intermediate Representationg
applyMatrix' m (Move dst mp) = Move (m !* dst) (applyMatrixMP m  mp)
applyMatrix' _ (Comment s) = Comment s
applyMatrix' _ Pause = Pause

-- | Applies a matrix transformation to an Intermediate Representation of a program.
applyMatrix :: V3 (V3 Double)		-- ^ The transformation matrix
		-> IR			-- ^ The Intermediate Representation
		-> IR			-- ^ The transformed Intermediate Representation
applyMatrix m = map (applyMatrix' m)

-- | Rotates 'IR' in the XY plane.
rotateIR :: 	Double 		-- ^ angle : The rotation angle
		-> IR		-- ^ The intermediate representation to rotate
		-> IR		-- ^ The transformed intermediate representation
rotateIR angle = applyMatrix m
	where 	m = V3 (V3 (cos radians) (-sin radians) 0) (V3 (sin radians) (cos radians) 0) (V3 0 0 1)
		radians = angle * pi / 180