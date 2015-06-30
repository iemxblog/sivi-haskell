{-|
Module		: Operation.Transformation
Description	: Functions to apply transformations to operations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Operation.Transformation (
	rotate
)
where

import IR
import Operation.Base
import Linear hiding (rotate)

-- | Applies a matrix transformation to move parameters.
applyMatrixMP :: M33 Double 	-- ^ m : The transformation matrix
		-> MoveParams	-- ^ The move parameters
		-> MoveParams	-- ^ The transformed move parameters
applyMatrixMP m Rapid = Rapid
applyMatrixMP m (LinearInterpolation f) = LinearInterpolation f
applyMatrixMP m (Arc dir center f) = Arc dir (m !* center) f

-- | This function applies a matrix transformation to an instruction (of Intermediate Representation)
applyMatrix' :: M33 Double		-- ^ m : The transformation matrix	
		-> Instruction		-- ^ p : The program in Intermediate Representation
		-> Instruction		-- ^ The resulting Intermediate Representationg
applyMatrix' m (Move dst mp) = Move (m !* dst) (applyMatrixMP m  mp)
applyMatrix' m (ChangeTool t) = ChangeTool t 
applyMatrix' m (Comment s) = Comment s
applyMatrix' m Pause = Pause

-- | Applies a matrix transformation to an Intermediate Representation of a program.
applyMatrix :: V3 (V3 Double)		-- ^ The transformation matrix
		-> IR			-- ^ The Intermediate Representation
		-> IR			-- ^ The transformed Intermediate Representation
applyMatrix m = map (applyMatrix' m)


-- | Rotates an operation in the XY plane.
rotate :: Double			-- ^ The rotation angle
	-> Operation IR			-- ^ The operation to rotate
	-> Operation IR			-- ^ The transformed operation
rotate angle op = op >>= (\ir -> return $ applyMatrix m ir)
	where m = V3 (V3 (cos angle) (sin angle) 0) (V3 (-sin angle) (cos angle) 0) (V3 0 0 1)
