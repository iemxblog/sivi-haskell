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

-- | Rotates an operation in the XY plane.
rotate :: Double			-- ^ angle : The rotation angle
	-> Operation IR			-- ^ op : The operation to rotate
	-> Operation IR			-- ^ The transformed operation
rotate angle op = op >>= (\ir -> return $ rotateIR angle ir)
