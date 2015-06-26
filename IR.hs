{-|
Module		: IR
Description	: Intermediate representation of a CNC program
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module IR 
(
	Tool(..)
	, ArcDirection(..)
	, MoveParams(..)
	, Instruction(..)
	, IR
	, compile
) where

import Linear
import Numeric

-- | Tool data type.
-- Used for tool changes, radius compensation.
data Tool = 
	EndMill { name :: String, diameter :: Double, len :: Double }
	| BallEndMill { name :: String, diameter :: Double, shankDiameter :: Double, len :: Double } 
	deriving (Eq, Show)

type Coordinate = Double
type FeedRate = Double

data ArcDirection = CW | CCW deriving (Eq, Show)

-- | Parameters for the 'Move' data constructor.
-- A move is either a Rapid move, or a linear interpolation with a feed rate. So there is only one data constructor for moves.
data MoveParams = Rapid 
		| LinearInterpolation { feedRate :: FeedRate } 
		| Arc { direction :: ArcDirection, center :: (V3 Double), feedRate :: FeedRate }
		deriving (Eq, Show)

-- | Intermediate Representation
data Instruction = 
	Move (V3 Coordinate) MoveParams
	| ChangeTool Tool
	| Comment String
	| Pause
	deriving (Eq, Show)

-- | A program is a list of instructions in intermediate representation.
type IR = [Instruction]


example :: IR
example = [
	ChangeTool (EndMill "01" 3 42)
	, Move (V3 1 0 0) Rapid
	, Move (V3 2 2 0) (LinearInterpolation 100)
	, Move (V3 2 0 0) (Arc CW (V3 1 1 0) 100)
	, Pause
	, Move (V3 0 0 0) Rapid,
	Comment "Commentaire"
	]


showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

-- | Helper function for "compile"
compile' :: IR		-- ^ The program to compile 
	-> V3 Double 	-- ^ The current position of the tool (needed for arcs, because i j k are relative to current position)
	-> [String]	-- ^ The gcode stored as a list of lines
compile' [] cp = []
compile' ((Move (V3 x y z) Rapid) : xs) cp = ["G00 X" ++ showDouble x ++ " Y" ++ showDouble y ++ " Z" ++ showDouble z] ++ compile' xs (V3 x y z)
compile' ((Move (V3 x y z) (LinearInterpolation f)) : xs) cp = ["G01 X" ++ showDouble x ++ " Y" ++ showDouble y ++ " Z" ++ showDouble z ++ " F" ++ showDouble f] ++ compile' xs (V3 x y z)
compile' ((Move (V3 x y z) (Arc dir center f)) : xs) cp = [g dir ++ " X" ++ showDouble x ++ " Y" ++ showDouble y ++ " Z" ++ showDouble z ++ notZero " I" i ++ notZero " J" j ++ notZero " K" k ++ " F" ++ showDouble f] ++ compile' xs (V3 x y z)
							where 	g CW = "G02"
								g CCW = "G03"
								V3 i j k = center - cp
								notZero s v = if v /= 0 then s ++ showDouble v else ""
compile' ((ChangeTool t) : xs) cp = ["M6 T" ++ name t] ++ compile' xs cp
compile' ((Comment s) : xs) cp = ["(" ++ s ++ ")"] ++ compile' xs cp
compile' (Pause : xs) cp = ["M00"] ++ compile' xs cp

-- | Compiles intermediate representation to G-CODE
compile :: IR 		-- ^ The program in intermediate representation
	 -> String 	-- ^ The generated G-Code
compile p = unlines $ compile' p (V3 0 0 0)
