{-|
Module		: Sivi.GCode.Base
Description	: GCode data structure
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.GCode.Base
(
	GCode(..)
) where

import Numeric

data GCode = 	G00 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double }
		| G01 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double, f :: Maybe Double }
		| G02 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
			i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
		| G03 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
			i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
		| GComment { getComment :: String }
		| M00
		| CLine { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
			i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
		| G38d2 { x:: Maybe Double, y :: Maybe Double, z :: Maybe Double, f :: Maybe Double }
		| G92 { x:: Maybe Double, y :: Maybe Double, z :: Maybe Double }

-- | Helper function used to show a Double with 3 decimals
showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

-- | Shows a GCode word (with Maybe value)
gword :: Char -> Maybe Double -> String
gword w (Just v) = w : showDouble v
gword _ Nothing = ""

-- | Compiles parameters of a GCode command
compileParams :: [Char] 		-- ^ List of parameter names
		-> [Maybe Double] 	-- ^ List of parameter values
		-> String		-- ^ Compiled parameters
compileParams pn pv = unwords . filter (/= "") $ zipWith gword pn pv

instance Show GCode where
	show (G00 mx my mz) = "G00 " ++ compileParams "XYZ" [mx, my, mz]
	show (G01 mx my mz mf) = "G01 " ++ compileParams "XYZF" [mx, my, mz, mf]
	show (G02 mx my mz mi mj mk mf) = "G02 " ++ compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
	show (G03 mx my mz mi mj mk mf) = "G03 " ++ compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
	show (GComment c) = "( " ++ c ++ " )"
	show M00 = "M00"
	show (CLine mx my mz mi mj mk mf) = compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
	show (G38d2 mx my mz mf) = "G38.2 " ++ compileParams "XYZF" [mx, my, mz, mf]
	show (G92 mx my mz) = "G92 " ++ compileParams "XYZ" [mx, my, mz]

