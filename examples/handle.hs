module Main (
	main
) where

import Sivi
import Linear


cut :: Double -> Double -> Operation IR
cut d l = 
	chain 5 [
		probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
		, zRepetition (-d) (Just 1) (saw_left d)
	]

drillings :: Double -> Double -> Double -> Double -> Operation IR
drillings d l d1 d2 = 
	withTool (EndMill 2 42) $
		-- add tool height probe
		chain 5 [
			translate (V3 d1 (d/2) 0) (zRepetition (-d) (Just 10) drill)
			, translate (V3 (l-d2) (d/2) 0) (zRepetition (-d) (Just 10) drill)
		]

axial_drilling :: Double -> Double -> Double -> Operation IR
axial_drilling dc dd l = 
	chain 5 [
		probeOuterCylinder dc 5 (ProbeTool 3 42)
		+++ zRepetition (-l) Nothing (circularPocket dd 0.5)
	]

handle :: Operation IR
handle = 
	chain 5 [
		cut d (l+2)
		+++ retract 30
		+++ comment "Please rotate the part to cut the other side"
		+++ pause
		+++ cut d l
		, drillings d l d1 d2
	]
	+++ axial_drilling d 4.5 l
	where
		l = 15
		d = 10
		d1 = 5
		d2 = 5

main :: IO ()
--main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ handle
main = interface . toGCode . runOperationWithDefaultParams $ handle
