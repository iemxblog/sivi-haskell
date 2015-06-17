module Main (
	main
) where

import Backlash
import IR
import Linear
import Operation

pos :: [V3 Double]
pos = [   V3 2 2 0
	, V3 3 3 0
	, V3 5 4 0
	, V3 7 3 0
	, V3 8 2 0 
	, V3 12 3 0 
	, V3 11 0 0
	, V3 10 (-1) 0
	, V3 7 (-1) 0
	, V3 6 0 0
	, V3 5 0 0
	, V3 4 0 0
	, V3 3 0 0
	, V3 2 (-1) 0
	]

initPos :: [V3 Double]
initPos = [V3 0 0 0, V3 1 1 1]

backlash :: V3 Double
backlash = V3 0.5 0.5 0.5

main :: IO()
main = putStr . compile $ backlashCompensation (map (\x -> Move x Rapid) pos) initPos backlash 
