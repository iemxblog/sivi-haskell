module Main (
	main
) where

import Sivi
import Linear

probeStrut :: Double -> Double -> Double -> Tool -> Operation IR
probeStrut d l margin probetool = 
	withTool probetool (
		comment "Place the probe 5mm above the right side of the strut, centered on the axis of the cylinder"	
		+++ pause
		+++ defCurPos (V3 l (d/2) 5)
		+++ chain 5 [
			probeZMinus (V3 l (d/2) 0) margin
			, probeXMinus (V3 l (d/2) (-d/2)) margin
			, probeYPlus (V3 0 0 (-d/2)) margin
		]
	)
	+++ comment "Don't forget to put the probe connectors for tool length measurement."
	+++ pause
	+++ probeZMinus (V3 0 (d/2) 0) margin
	+++ comment "Remove the probe connectors"
	+++ pause

strut' :: Double -> Double -> Operation IR
strut' d l = 
	chain 5 [
		probeStrut d l 5 (ProbeTool 3 42)
		, zRepetition (-d) (Just 1) (saw_left d)
	]

strut :: Double -> Double -> Operation IR
strut d l = 
	strut' (d+2) l
	+++ retract 30
	+++ comment "Please rotate the strut to machine the other side"
	+++ pause
	+++ strut' d l 

op :: Operation IR
op = strut 8 30
		
main :: IO ()
--main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ op
main = interface . toGCode . runOperationWithDefaultParams $ op
