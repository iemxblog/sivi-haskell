{-|
Module		: Sivi.Range
Description	: Custom range function
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}

module Sivi.Range (
	range
) where


-- | Range which ends exactly at the end value : last (range start end step) == end
-- Used in 'zRepetition' and 'arcInterpolation'.
range :: (Eq a, Num a) => a 		-- ^ start (also the accumulator used for recursion)
			-> a		-- ^ end
			-> a		-- ^ step
			-> [a]		-- ^ [start, start+step, start+2*step, ..., end]
range start end step 	| signum (end - start) == signum step = start : range (start+step) end step 
			| otherwise = [end]

