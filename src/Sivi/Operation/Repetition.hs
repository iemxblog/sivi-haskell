{-|
Module		: Sivi.Operation.Repetition
Description	: Operations that repeat other operations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Repetition (
	circularRepetition
	, gridRepetition
	, zRepetition
) where

import Sivi.Operation.Base
import Sivi.IR
import Sivi.Range
import Linear hiding (rotate)

repetition :: [V3 Double] -> Double -> Operation IRTree -> Operation IRTree
repetition xs z_safe op = chain z_safe $ map (\v -> translate v op) xs

repetitionWithoutRetract :: [V3 Double] -> Operation IRTree -> Operation IRTree
repetitionWithoutRetract xs op = opsequence $ map (\v -> translate v op) xs

-- | Repeats an operation on a circle (and adds a tool retraction each time)
circularRepetition :: Double			-- ^ d : Diameter 
			-> Int			-- ^ n : Number of repetitions
			-> Double		-- ^ start_angle : Start angle
			-> Double		-- ^ z_safe : Tool retraction 
			-> Operation IRTree	-- ^ op : Operation to repeat
			-> Operation IRTree	-- ^ Operation 1 on circle + Retraction + Operation 2 on circle + ...
circularRepetition d n start_angle z_safe op = chain z_safe . map (\a -> rotate a . translate (V3 (d/2) 0 0) $ op) $ angles 
	where
		angles = [fromIntegral i * 360/fromIntegral n + start_angle | i <- [0..n-1]]

-- | Repeats an operation on a grid (and adds a tool retraction each time)
gridRepetition :: Int			-- ^ nx : Number of repetitions on X axis
		 -> Int			-- ^ ny : Number of repetitions on Y axis
		 -> Double		-- ^ space_x : Space between each operation on X axis
		 -> Double		-- ^ space_y : Space between each operation on Y axis
		 -> Double		-- ^ Tool retraction
		 -> Operation IRTree	-- ^ Operation to repeat
		 -> Operation IRTree	-- ^ Operation 1 on grid + Retraction + Operation 2 on grid + ...
gridRepetition nx ny space_x space_y = repetition pos_list
	where
		pos_list = [V3 (fromIntegral x * space_x) (fromIntegral y * space_y) 0 | x <- [0..nx-1], y <- [0..ny-1]]

-- | Ensures that a number is negative. Used for 'zRepetition' depth.
negative :: (Num a, Ord a) => a -> a
negative x = if x <= 0 then x else (-x)

-- | Repeats an operation on the z axis (possible to add a tool retraction between each pass)
zRepetition :: Double			-- ^ depth : Depth of the final pass (relative to the first)
		-> Maybe Double		-- ^ m_z_safe : Adds tool retraction to z_safe between each pass (if provided)
		-> Operation IRTree	-- ^ op : Operation to repeat
		-> Operation IRTree	-- ^ Resulting operation
zRepetition depth m_z_safe op = do
	step <- getDepthOfCut
	let pos_list = map (V3 0 0) $ range step (negative depth) step
	case m_z_safe of
		Just z_safe -> repetition pos_list z_safe op
		Nothing -> repetitionWithoutRetract pos_list op
