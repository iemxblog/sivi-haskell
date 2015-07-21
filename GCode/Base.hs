{-|
Module		: GCode.Base
Description	: GCode data structure
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module GCode.Base
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
		| Comment { getComment :: String }
		| M00
		| CLine { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
			i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }

-- | Helper function used to show a Double with 3 decimals
showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

-- | Shows a GCode word (with Maybe value)
gword :: Char -> Maybe Double -> String
gword w (Just v) = w : showDouble v
gword w Nothing = ""

-- | Compiles parameters of a GCode command
compileParams :: [Char] 		-- ^ List of parameter names
		-> [Maybe Double] 	-- ^ List of parameter values
		-> String		-- ^ Compiled parameters
compileParams pn pv = unwords . filter (/= "") $ (zipWith gword pn pv)

instance Show GCode where
	show (G00 mx my mz) = "G00 " ++ compileParams ['X', 'Y', 'Z'] [mx, my, mz]
	show (G01 mx my mz mf) = "G01 " ++ compileParams ['X', 'Y', 'Z', 'F'] [mx, my, mz, mf]
	show (G02 mx my mz mi mj mk mf) = "G02 " ++ compileParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F'] [mx, my, mz, mi, mj, mk, mf]
	show (G03 mx my mz mi mj mk mf) = "G03 " ++ compileParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F'] [mx, my, mz, mi, mj, mk, mf]
	show (Comment c) = "( " ++ c ++ " )"
	show M00 = "M00"
	show (CLine mx my mz mi mj mk mf) = compileParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F'] [mx, my, mz, mi, mj, mk, mf]


example :: [GCode]
example = [	G00 (Just 1) (Just 2) (Just 3), 
		G01 (Just 10) (Just 0) Nothing (Just 100), 
		G02 (Just 10) (Just 0) Nothing (Just (-1)) (Just (-2)) Nothing Nothing,
		G03 (Just 10) (Just 0) Nothing (Just (-1)) (Just (-2)) Nothing Nothing,
		Comment "comment",
		M00,
		CLine (Just 20) (Just 30) Nothing Nothing Nothing Nothing (Just 30)
	]
	
