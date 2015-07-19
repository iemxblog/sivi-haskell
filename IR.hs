{-# LANGUAGE OverloadedStrings #-}
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
	, testProgram
	, memorizedParams
	, memorizeParams
	, memorizedCommands
	, memorizeCommand
) where

import Linear
import Numeric
import qualified Data.Map as Map
import GCode.Base
import GCode.Parser
import Data.Attoparsec.ByteString.Char8

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
	IR.Comment "Commentaire"
	]


showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

-- | Helper function for "compile"
compile' :: IR		-- ^ The program to compile 
	-> V3 Double 	-- ^ The current position of the tool (needed for arcs, because i j k are relative to current position)
	-> [GCode]	-- ^ The resulting 'GCode'
compile' [] cp = []
compile' ((Move (V3 x y z) Rapid) : xs) cp = G00 (Just x) (Just y) (Just z) : compile' xs (V3 x y z)
compile' ((Move (V3 x y z) (LinearInterpolation f)) : xs) cp = G01 (Just x) (Just y) (Just z) (Just f) : compile' xs (V3 x y z)
compile' ((Move (V3 x y z) (Arc dir center f)) : xs) cp = g dir (Just x) (Just y) (Just z) (notZero i) (notZero j) (notZero k) (Just f) : compile' xs (V3 x y z)
							where 	g CW = G02
								g CCW = G03
								V3 i j k = center - cp
								notZero v = if v /= 0 then Just v else Nothing
compile' ((ChangeTool t) : xs) cp = M06 (name t) : compile' xs cp
compile' ((IR.Comment s) : xs) cp = GCode.Base.Comment s : compile' xs cp
compile' (Pause : xs) cp = M00 : compile' xs cp

-- | Compiles intermediate representation to G-CODE
compile :: IR 		-- ^ The program in intermediate representation
	 -> [GCode] 	-- ^ The generated G-Code
compile p = compile' p (V3 0 0 0)

filterNothing :: Eq b => Map.Map a (Maybe b) -> Map.Map a b
filterNothing = Map.map f . Map.filter (/= Nothing)
	where f (Just x) = x

-- | Helper function for 'memorizeParams'
mem :: (Ord a, Eq b) => [(a, Maybe b)] -> Map.Map a b -> Map.Map a b
mem xs pm = Map.union (filterNothing . Map.fromList $ xs) pm 

memorizeParams :: Map.Map Char Double
		-> GCode
		-> Map.Map Char Double
memorizeParams pm (G00 mx my mz) = mem [('X', mx), ('Y', my), ('Z', mz)] pm
memorizeParams pm (G01 mx my mz mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('F', mf)] pm
memorizeParams pm (G02 mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm
memorizeParams pm (G03 mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm
memorizeParams pm (M06 _) = pm
memorizeParams pm (GCode.Base.Comment _) = pm
memorizeParams pm M00 = pm
memorizeParams pm (CLine mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm

memorizeCommand :: String -> GCode -> String
memorizeCommand _ (G00 _ _ _) = "G00"
memorizeCommand _ (G01 _ _ _ _) = "G01"
memorizeCommand _ (G02 _ _ _ _ _ _ _) = "G02"
memorizeCommand _ (G03 _ _ _ _ _ _ _) = "G03"
memorizeCommand s (M06 _) = s
memorizeCommand s (GCode.Base.Comment _) = s
memorizeCommand s M00 = s
memorizeCommand s (CLine _ _ _ _ _ _ _) = s

memorizedCommands :: [GCode] -> [String]
memorizedCommands = tail . scanl memorizeCommand ""

memorizedParams :: [GCode] -> [Map.Map Char Double]
memorizedParams = tail . scanl memorizeParams (Map.fromList [('X', 0), ('Y', 0), ('Z', 0), ('I', 0), ('J', 0), ('K', 0), ('F', 0)]) 

--testProgram :: [GCode]
testProgram = case parseOnly parser "G00 X1 Z2\nG01 Y2 F100\nX3\nY4\n" of
		Left err -> []
		Right gcode -> gcode

getParams :: Map.Map Char Double -> [Char] -> [Double]
getParams mp = map $ f . (flip Map.lookup $ mp)
	where
		f (Just x) = x
		f Nothing = error "Error in getParams. Probably due to a bug."

previousPositions :: [GCode] -> [V3 Double]
previousPositions = (V3 0 0 0 :) . init . map ( f . (flip getParams) "XYZ" ) . memorizedParams
	where f [x, y, z] = V3 x y z

