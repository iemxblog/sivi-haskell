module Main (
	main
) where

import Sivi
import Linear

d1 = 10 + 0.3
d2 = 14 + 0.4
d3 = 20 - 0.2
d4 = 26
h = 10
e1 = 1
e2 = 3

cap :: Bool -> Operation IR
cap hole = chain 1 [
		circularPocket d2 (h-e1) 0.5
		, if hole then translate (V3 0 0 (-h+e1)) (circularPocket d1 e1 0.5) else noOp
		, cylinderOuter d3 (h-e2)
		, translate (V3 0 0 (-h+e2)) (cylinderOuter d4 e2)
	]


main :: IO ()
main = putStr . (++"M2\n") . toString . runOperation (100, 30, 10, (-1)) (V3 0 0 0) EndMill{diameter=3, len=42} $ cap True
--main = interface . toGCode . runOperation (100, 30, 10, (-1)) (V3 0 0 0) EndMill{diameter=3, len=42} $ cap True
