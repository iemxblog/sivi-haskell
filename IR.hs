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
	, MoveParams(..)
	, IR(..)
	, Program
	, compile
) where

import Linear

-- | Tool data type.
-- Used for tool changes, radius compensation.
data Tool = 
	EndMill { name :: String, diameter :: Double, length :: Double }
	| BallEndMill { name :: String, diameter :: Double, shankDiameter :: Double, length :: Double } 
	deriving (Eq, Show)

type Coordinate = Double
type FeedRate = Double

-- | Parameters for the 'Move' data constructor.
-- A move is either a Rapid move, or a linear interpolation with a feed rate. So there is only one data constructor for moves.
data MoveParams = Rapid | LinearInterpolation { feedRate :: FeedRate } deriving (Eq, Show)

-- | Intermediate Representation
data IR = 
	Move (V3 Coordinate) MoveParams
	| ChangeTool Tool
	| Comment String
	| Pause
	deriving (Eq, Show)

-- | A program is a list of instructions in intermediate representation.
type Program = [IR]


example :: Program
example = [
	ChangeTool (EndMill "01" 3 42)
	, Move (V3 1 0 0) Rapid
	, Move (V3 2 2 0) (LinearInterpolation 100)
	, Pause
	, Move (V3 0 0 0) Rapid,
	Comment "Commentaire"
	]

-- | Helper function for "compile"
compileIR :: IR -> String
compileIR (Move (V3 x y z) Rapid) = "G00 X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z
compileIR (Move (V3 x y z) (LinearInterpolation f)) = "G01 X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z ++ " F" ++ show f
compileIR (ChangeTool t) = "M6 T" ++ name t
compileIR (Comment s) = "(" ++ s ++ ")"
compileIR Pause = "M00"

-- | Compiles intermediate representation to G-CODE
compile :: Program -- ^ The program in intermediate representation
	 -> String -- ^ The generated G-Code
compile p = unlines $ map compileIR p
