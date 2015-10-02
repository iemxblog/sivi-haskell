module Main (
	main

) where

import Sivi
import Linear hiding (rotate)
import ScrewHoles

corner = V3 (-20) 5 0

bearing = chain 1 [
		circularPocket 14 2.5 0.5
		, withTool (EndMill 2 42) $ probeZMinus (V3 15 0 0) 5 +++ cylinderInner 20 4
		, probeZMinus (V3 15 0 0) 5
		, translate (V3 0 0 (-2.5)) $ circularPocket 6.5 (11-2.5) 0.5
	]

makeProbePocket = translate (V3 (-25) 0 0) $ rectangularPocket 10 10 10 0.5

makeAlignmentPocket = translate (V3 70 0 0) $ rectangularPocket 10 10 10 0.5

back = chain 1 [ 
		comment "Place the tool above the location of the bearing and start the spindle"
		, pause
		, makeProbePocket
		, makeAlignmentPocket
		, comment "Stop the spindle"
		, pause
		, translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)
		, comment "Start the spindle"
		, pause
		, bearing
	]

half = chain 1 [
		translate (V3 9 (-28.6) 0) $ drill 10 1
		, translate (V3 54.2 (-19.8) 0) $ drill 10 1
		, zRepetition 10.5 (Just 1) $ contour [V2 (-14.5) 3, V2 (-14.5) (-35), V2 22.75 (-35), V2 62.9 (-24.38), V2 62.9 3] RightSide False
	]

front1 = chain 1 [
		translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)
		, half
	]	

front2 = chain 1 [
		translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)
		, bearing
		, rotate (-90) screwHoles
	]

front3 = chain 1 [
		translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)
		, symmetryX half
	]

zPlate = chain 1 [
	comment "Begin with the back side of the plate"
	, pause
	, back
	, comment "Place the front side face up"
	, pause
	, front1
	, comment "Place the part to machine the second part of the front side"
	, pause
	, front2
	, comment "Place the part to machine the third part of the front side"
	, pause
	, front3
	]

main :: IO ()
main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ zPlate
--main = interface . toGCode . runOperationWithDefaultParams $ zPlate
