module Main (
	main
) where

import Sivi
import Linear


cut :: Double -> Double -> Operation IRTree
cut d l = 
	chain 5 [
		probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
		, saw_left d d 1
	]

drillings :: Double -> Double -> Double -> Double -> Operation IRTree
drillings d l d1 d2 = 
	withTool (EndMill 2 42) $
		chain 5 [
			probeZMinus (V3 d1 (d/2) 0) 5
			, comment "Start the spindle" +++ pause
			, translate (V3 d1 (d/2) 0) (drill (d+1) 10)
			, translate (V3 (l-d2) (d/2) 0) (drill (d+1) 10)
		]

axialDrilling :: Double -> Double -> Double -> Operation IRTree
axialDrilling dc dd l = 
	chain 5 [
		probeOuterCylinder dc 5 (ProbeTool 3 42)
		+++ circularPocket dd l 0.5
	]

handle :: Operation IRTree
handle = 
	chain 5 [
		cut d (l+2)
		+++ retract 30
		+++ comment "Please rotate the part to cut the other side"
		+++ pause
		+++ cut d l
		, drillings d l d1 d2
	]
	+++ axialDrilling d 4.5 l
	where
		l = 12
		d = 10
		d1 = 7.2
		d2 = 3

main :: IO ()
--main = putStr . (++"M2\n") . toString . flatten . runOperationWithDefaultParams $ handle
main = interface . toGCode . flatten . runOperationWithDefaultParams $ handle
