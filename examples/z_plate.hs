module Main (
	main

) where

import Sivi
import Linear hiding (rotate)
import ScrewHoles

corner = V3 (-40) 5 0
probeCorner = translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)

bearing = chain 1 [
		circularPocket 14 2.5 0.5
		, withTool (EndMill 2 42) $ probeZMinus (V3 15 0 0) 5 +++ cylinderInner 20 4
		, probeZMinus (V3 15 0 0) 5
		, translate (V3 0 0 (-2.5)) $ circularPocket 6.5 (11-2.5) 0.5
	]

makeProbePocket = translate (V3 (-45) 0 0) $ rectangularPocket 10 10 (10+0.5) 0.5

makeAlignmentPocket = translate (V3 45 0 0) $ rectangularPocket 10 10 (10+0.5) 0.5

back = chain 1 [ 
		message "Place the tool above the location of the bearing and start the spindle"
		, makeProbePocket
		, makeAlignmentPocket
		, message "Stop the spindle"
		, translate corner $ probeInnerCornerNE 5 (ProbeTool 3 42)
		, message "Start the spindle"
		, bearing
	]

front1 = chain 1 [
		probeCorner
		, bearing
		, screwHoles
		, translate (V3 28.6 9 0) $ drill (10+0.5) 1
		, translate (V3 (-28.6) 9 0) $ drill (10+0.5) 1
		, zRepetition 10.5 (Just 1) $ contour [V2 (-35) 22.75, V2 (-35) (-14.5), V2 35 (-14.5), V2 35 22.75] RightSide False
	]

front2 = chain 1 [
		probeCorner
		, translate (V3 19.8 54.2 0) $ drill (10+0.5) 1
		, translate (V3 (-19.8) 54.2 0) $ drill (10+0.5) 1
		, zRepetition 10.5 (Just 1) $ contour [V2 35 22.75, V2 24.38 62.9, V2 (-24.38) 62.9, V2 (-35) 22.75] RightSide False
	]

zPlate = chain 1 [
	message "Begin with the back side of the plate"
	, back
	, message "Place the front side face up, centered on the lower area of the part"
	, front1
	, message "Place the part to machine the upper area of the front side"
	, pause
	, front2
	]

main :: IO ()
main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ zPlate
--main = interface . toGCode . runOperationWithDefaultParams $ zPlate
