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
) where

import Control.Monad.State
import qualified Data.Map as Map
import Linear
import Sivi.IR.Base
import Sivi.GCode

-- | Monadic datatype to memorize GCode commands (as String) and parameters (as Data.Map.Map Char Double)
type GCodeTransformer a = State (String, Map.Map Char Double) a

getParamsMap :: GCodeTransformer (Map.Map Char Double)
getParamsMap = liftM snd get

getCommand :: GCodeTransformer String
getCommand = liftM fst get

putParam :: Char -> Maybe Double -> GCodeTransformer ()
putParam _ Nothing = return ()
putParam pn (Just v) = do
			(c, ps) <- get
			put (c, Map.insert pn v ps)

putParams :: [Char] -> [Maybe Double] -> GCodeTransformer ()
putParams pns pvs = mapM_ (uncurry putParam) $ zip pns pvs

putCommand :: String -> GCodeTransformer ()
putCommand c = do
		ps <- getParamsMap
		put (c, ps)

getParam :: Char -> GCodeTransformer Double
getParam pn = do
		ps <- getParamsMap
		case Map.lookup pn ps of
			Nothing -> error $ "getParam : Unknow parameter " ++ [pn]	
			Just pv -> return pv

getParams :: [Char] -> GCodeTransformer [Double]
getParams = mapM getParam
			

-- | Transforms a 'GCode' instruction to an 'IR' instruction
fromGCode' :: GCode -> GCodeTransformer Instruction
fromGCode' (G00 x y z) = do
				putParams "XYZ" [x,y,z]
				putCommand "G00"
				[x,y,z] <- getParams "XYZ"
				return $ Move (V3 x y z) Rapid
fromGCode' (G01 x y z f) = do
				putParams "XYZF" [x,y,z,f]
				putCommand "G01"
				[x,y,z,f] <- getParams "XYZF"
				return $ Move (V3 x y z) (LinearInterpolation f)	
fromGCode' (G02 x y z i j k f) = do
					[px, py, pz] <- getParams "XYZ" -- previous position
					putParams "XYZIJKF" [x,y,z,i,j,k,f]
					putCommand "G02"
					[x,y,z,i,j,k,f] <- getParams "XYZIJKF"
					return $ Move (V3 x y z) (Arc CW (V3 px py pz + V3 i j k) f)
fromGCode' (G03 x y z i j k f) = do
					[px, py, pz] <- getParams "XYZ" -- previous position
					putParams "XYZIJKF" [x,y,z,i,j,k,f]
					putCommand "G03"
					[x,y,z,i,j,k,f] <- getParams "XYZIJKF"
					return $ Move (V3 x y z) (Arc CCW (V3 px py pz + V3 i j k) f)
fromGCode' (Sivi.GCode.GComment s) = return $ Comment s
fromGCode' M00 = return Pause
fromGCode' (G38d2 x y z f) = do
				putParams "XYZF" [x,y,z,f]
				putCommand "G38.2"
				[x,y,z,f] <- getParams "XYZF"
				return $ Move (V3 x y z) (Probe f)
fromGCode' (G92 x y z) = do
				putParams "XYZ" [x, y, z]
				-- no putCommand !
				[x,y,z] <- getParams "XYZ"
				return $ DefCurPos (V3 x y z)
fromGCode' (CLine x y z i j k f) = do
					c <- getCommand
					case c of
						"G00" -> fromGCode' (G00 x y z) 	
						"G01" -> fromGCode' (G01 x y z f)
						"G02" -> fromGCode' (G02 x y z i j k f)
						"G03" -> fromGCode' (G03 x y z i j k f)
						"G38.2" -> fromGCode' (G38d2 x y z f)
						_ -> error $ "fromGCode' : Unknown memorized command (" ++ c ++ ")"

-- | Transforms GCode to Intermediate Representation
fromGCode :: [GCode] -> IR
fromGCode xs = evalState (mapM fromGCode' xs) ("", Map.fromList (zip "XYZIJKF" (repeat 0)))
