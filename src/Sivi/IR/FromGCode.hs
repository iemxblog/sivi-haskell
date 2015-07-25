{-# LANGUAGE OverloadedStrings #-}
{-|
Module		: Sivi.IR.FromGCode
Description	: Conversion of GCode to IR
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.IR.FromGCode
(
	fromGCode	
	, testProgram
) where

import Linear
import qualified Data.Map as Map
import Data.List
import Sivi.IR.Base
import Sivi.GCode

-- | Takes a Map a (Maybe b), removes 'Nothing' values, and removes the 'Just' data constructor.
filterNothing :: Eq b => Map.Map a (Maybe b) -> Map.Map a b
filterNothing = Map.map f . Map.filter (/= Nothing)
	where f (Just x) = x

-- | Helper function for 'memorizeParams'
mem :: (Ord a, Eq b) => [(a, Maybe b)] -> Map.Map a b -> Map.Map a b
mem xs = Map.union (filterNothing . Map.fromList $ xs)

-- | Helper function for 'memorizedParams'
memorizeParams :: Map.Map Char Double	-- ^ The parameters of the previous instruction
		-> GCode		-- ^ The current instruction (with its parameters inside, and some parameters that are not mentioned because optional)
		-> Map.Map Char Double	-- ^ All the parameters of the current instruction
memorizeParams pm (G00 mx my mz) = mem [('X', mx), ('Y', my), ('Z', mz)] pm
memorizeParams pm (G01 mx my mz mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('F', mf)] pm
memorizeParams pm (G02 mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm
memorizeParams pm (G03 mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm
memorizeParams pm (Sivi.GCode.GComment _) = pm
memorizeParams pm M00 = pm
memorizeParams pm (G38d2 mx my mz mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('F', mf)] pm
memorizeParams pm (G92 mx my mz) = mem [('X', mx), ('Y', my), ('Z', mz)] pm	-- particular case : we memorize the params, but not the command
memorizeParams pm (CLine mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm

-- | Helper fuunction for 'memorizedCommands'
memorizeCommand :: 	String 		-- ^ The previous memorized command
			-> GCode	-- ^ The current GCode instruction 
			-> String	-- ^ The new memorized GCode command
memorizeCommand _ (G00 _ _ _) = "G00"
memorizeCommand _ (G01 _ _ _ _) = "G01"
memorizeCommand _ (G02 _ _ _ _ _ _ _) = "G02"
memorizeCommand _ (G03 _ _ _ _ _ _ _) = "G03"
memorizeCommand s (Sivi.GCode.GComment _) = s
memorizeCommand s M00 = s
memorizeCommand _ (G38d2 _ _ _ _) = "G38.2"
memorizeCommand s (G92 _ _ _) = s		-- particular case : we memorize the params, but not the command
memorizeCommand s (CLine _ _ _ _ _ _ _) = s

-- | When we do a G00 X1 Y2 Z3, it is not mandatory to mention the command in the next line, for example : X2 Y3. So we need to memorize commands. memorizedCommands generates a list of commands for each instruction of the program.
memorizedCommands :: 	[GCode] 	-- ^ The GCode program
			-> [String]	-- ^ The memorized intruction for each line of the program.
memorizedCommands = tail . scanl memorizeCommand ""

-- | GCode parameters are optional. So we need to keep track of previous values. So memorizeParams generates a list of the parameters values for each instruction of the GCode program given as parameter.
memorizedParams :: [GCode]			-- ^ The GCode program
		-> [Map.Map Char Double]	-- ^ The memorized parameters for each line of the program
memorizedParams = tail . scanl memorizeParams (Map.fromList [('X', 0), ('Y', 0), ('Z', 0), ('I', 0), ('J', 0), ('K', 0), ('F', 0)]) 

-- | Example. To remove !!!!
testProgram :: [GCode]
testProgram = case parse "G00 X1 Z2\nG01 Y2 F100\nX3\nY4\nX2 Y2 Z0\nG02 X4 I1 J-1\nG38.2 X-10 F10\nY-10\nG92 X0 Y0\nZ-10" of
		Left err -> []
		Right gcode -> gcode

-- | Generates a list of values from a Map.
getParams :: 	Map.Map Char Double	-- ^ mp : Parameter names and their values stored in a Map
		-> [Char] 		-- ^ Names of the parameters we want to get
		-> [Double]		-- ^ Values of the parameters we want to get, in the order of the names given as parameter
getParams mp = map $ f . (flip Map.lookup mp)
	where
		f (Just x) = x
		f Nothing = error "Error in getParams. Probably due to a bug."

-- | Calculates the previous position of the tool for each line of the program.
previousPositions :: 	[GCode] 	-- ^ The GCode program
			-> [V3 Double]	-- ^ The previous positions of the tool
previousPositions = (V3 0 0 0 :) . init . map ( f . flip getParams "XYZ" ) . memorizedParams
	where f [x, y, z] = V3 x y z

-- | Helper function for 'fromGCode'. Translates a single GCode instruction to an IR 'Instruction'.
fromGCode' :: 	GCode 			-- ^ GCode to translate to an Instruction
		-> String 		-- ^ s : Memorized command
		-> Map.Map Char Double	-- ^ mp : Memorized parameters
		-> V3 Double		-- ^ pp : Previous position (used only for 'Arc')
		-> Instruction		-- ^ The resulting Instruction
fromGCode' (G00 _ _ _) _ mp _ = Move (V3 x y z) Rapid
			where
				[x, y, z] = getParams mp "XYZ"
fromGCode' (G01 _ _ _ _) _ mp _ = Move (V3 x y z) (LinearInterpolation f)
			where
				[x, y, z, f] = getParams mp "XYZF"
fromGCode' (G02 _ _ _ _ _ _ _) _ mp pp = Move (V3 x y z) (Arc CW (pp + V3 i j k) f)
			where
				[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"

fromGCode' (G03 _ _ _ _ _ _ _) _ mp pp = Move (V3 x y z) (Arc CCW (pp + V3 i j k) f)
			where
				[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"

fromGCode' (Sivi.GCode.GComment s) _ _ _ = Sivi.IR.Base.Comment s

fromGCode' M00 _ _ _ = Pause

fromGCode' (G38d2 _ _ _ _) _ mp _ = Move (V3 x y z) (Probe f)
			where
				[x, y, z, f] = getParams mp "XYZF"

fromGCode' (G92 _ _ _) _ mp _ = DefCurPos (V3 x y z)
			where
				[x, y, z] = getParams mp "XYZ"

fromGCode' (CLine _ _ _ _ _ _ _) mc mp pp = 
	case mc of
		"G00" -> Move (V3 x y z) Rapid
		"G01" -> Move (V3 x y z) (LinearInterpolation f)
		"G02" -> Move (V3 x y z) (Arc CW (pp + V3 i j k) f)
		"G03" -> Move (V3 x y z) (Arc CCW (pp + V3 i j k) f)
		"G38.2" -> Move (V3 x y z) (Probe f)
		otherwise -> error "Unknown memorized command. Probably due to a bug during GCode to IR conversion."
	where
		[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"

-- | Translates GCode to 'IR' (Intermediate Representation)
fromGCode :: 	[GCode]	-- ^ The GCode program
		 -> IR		-- ^ The resulting intermediate representation.
fromGCode xs = [fromGCode' gi mc mp pp | (gi, mc, mp, pp) <- zip4 xs (memorizedCommands xs) (memorizedParams xs) (previousPositions xs)]
