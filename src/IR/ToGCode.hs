{-|
Module		: IR.ToGCode
Description	: Conversion of IR to GCode
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module IR.ToGCode
(
	toGCode
	, toString
) where

import Linear
import Numeric
import IR.Base
import GCode 

showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

-- | Helper function for "compile"
toGCode' :: IR		-- ^ The program to compile 
	-> V3 Double 	-- ^ The current position of the tool (needed for arcs, because i j k are relative to current position)
	-> [GCode]	-- ^ The resulting 'GCode'
toGCode' [] cp = []
toGCode' ((Move (V3 x y z) Rapid) : xs) cp = G00 (Just x) (Just y) (Just z) : toGCode' xs (V3 x y z)
toGCode' ((Move (V3 x y z) (LinearInterpolation f)) : xs) cp = G01 (Just x) (Just y) (Just z) (Just f) : toGCode' xs (V3 x y z)
toGCode' ((Move (V3 x y z) (Arc dir center f)) : xs) cp = g dir (Just x) (Just y) (Just z) (notZero i) (notZero j) (notZero k) (Just f) : toGCode' xs (V3 x y z)
							where 	g CW = G02
								g CCW = G03
								V3 i j k = center - cp
								notZero v = if v /= 0 then Just v else Nothing
toGCode' ((IR.Base.Comment s) : xs) cp = GCode.Comment s : toGCode' xs cp
toGCode' (Pause : xs) cp = M00 : toGCode' xs cp

-- | Compiles intermediate representation to 'GCode'
toGCode :: IR 		-- ^ The program in intermediate representation
	 -> [GCode] 	-- ^ The generated G-Code
toGCode p = toGCode' p (V3 0 0 0)

-- | Compiles 'IR' (intermediate representation) to a GCode string
toString :: 	IR 		-- ^ The program in intermediate representation
		-> String	-- ^ The string containing GCode instructions
toString = unlines . map show . toGCode
