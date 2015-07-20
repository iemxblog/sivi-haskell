{-# LANGUAGE OverloadedStrings #-}
{-|
Module		: IR.FromGCode
Description	: Conversion of GCode to IR
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module IR.FromGCode
(
	fromGCode	
) where

import Linear
import qualified Data.Map as Map
import Data.List
import Data.Attoparsec.ByteString.Char8
import IR.Base
import GCode

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
memorizeParams pm (GCode.Comment _) = pm
memorizeParams pm M00 = pm
memorizeParams pm (CLine mx my mz mi mj mk mf) = mem [('X', mx), ('Y', my), ('Z', mz), ('I', mi), ('J', mj), ('K', mk), ('F', mf)] pm

memorizeCommand :: String -> GCode -> String
memorizeCommand _ (G00 _ _ _) = "G00"
memorizeCommand _ (G01 _ _ _ _) = "G01"
memorizeCommand _ (G02 _ _ _ _ _ _ _) = "G02"
memorizeCommand _ (G03 _ _ _ _ _ _ _) = "G03"
memorizeCommand s (M06 _) = s
memorizeCommand s (GCode.Comment _) = s
memorizeCommand s M00 = s
memorizeCommand s (CLine _ _ _ _ _ _ _) = s

memorizedCommands :: [GCode] -> [String]
memorizedCommands = tail . scanl memorizeCommand ""

memorizedParams :: [GCode] -> [Map.Map Char Double]
memorizedParams = tail . scanl memorizeParams (Map.fromList [('X', 0), ('Y', 0), ('Z', 0), ('I', 0), ('J', 0), ('K', 0), ('F', 0)]) 

testProgram :: [GCode]
testProgram = case parseOnly parser "G00 X1 Z2\nG01 Y2 F100\nX3\nY4\nX2 Y2 Z0\nG02 X4 I1 J-1\nM06 T01" of
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

fromGCode' :: 	GCode 			-- ^ GCode to translate to an Instruction
		-> String 		-- ^ s : Memorized command
		-> Map.Map Char Double	-- ^ mp : Memorized parameters
		-> V3 Double		-- ^ pp : Previous position (used only for 'Arc')
		-> [Tool]		-- ^ ts : List of tools
		-> Instruction		-- ^ The resulting Instruction
fromGCode' (G00 _ _ _) _ mp _ _ = Move (V3 x y z) Rapid
			where
				[x, y, z] = getParams mp "XYZ"
fromGCode' (G01 _ _ _ _) _ mp _ _ = Move (V3 x y z) (LinearInterpolation f)
			where
				[x, y, z, f] = getParams mp "XYZF"
fromGCode' (G02 _ _ _ _ _ _ _) _ mp pp _ = Move (V3 x y z) (Arc CW (pp + V3 i j k) f)
			where
				[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"

fromGCode' (G03 _ _ _ _ _ _ _) _ mp pp _ = Move (V3 x y z) (Arc CCW (pp + V3 i j k) f)
			where
				[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"

fromGCode' (M06 tn) _ mp _ ts = case find (\t -> name t == tn) ts of
					Just t -> ChangeTool t
					Nothing -> error $ "Tool " ++ show tn ++ " not found."

fromGCode' (GCode.Comment s) _ _ _ _ = IR.Base.Comment s

fromGCode' M00 _ _ _ _ = Pause

fromGCode' (CLine _ _ _ _ _ _ _) mc mp pp _ = 
	case mc of
		"G00" -> Move (V3 x y z) Rapid
		"G01" -> Move (V3 x y z) (LinearInterpolation f)
		"G02" -> Move (V3 x y z) (Arc CW (pp + V3 i j k) f)
		"G03" -> Move (V3 x y z) (Arc CCW (pp + V3 i j k) f)
		otherwise -> error "Unknown memorized command. Probably due to a bug during GCode to IR conversion."
	where
		[x, y, z, i, j, k, f] = getParams mp "XYZIJKF"


fromGCode :: [Tool] -> [GCode] -> IR
fromGCode ts xs = [fromGCode' gi mc mp pp ts | (gi, mc, mp, pp) <- zip4 xs (memorizedCommands xs) (memorizedParams xs) (previousPositions xs)]
