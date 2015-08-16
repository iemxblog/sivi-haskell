module Main (
	main
) where

import Sivi
import Linear hiding (rotate)

probeStrut :: Double -> Double -> Double -> Tool -> Operation IR
probeStrut d l margin probetool = 
	( withTool probetool $ 
		comment "Place the probe 5mm above the right side of the strut, centered on the axis of the cylinder"	
		+++ pause
		+++ defCurPos (V3 l (d/2) 5)
		+++ probeZMinus (V3 l (d/2) 0) margin
		+++ probeXMinus (V3 l (d/2) (-d/2)) margin
		+++ retract 5
		+++ probeYPlus (V3 0 0 (-d/2)) margin
	)
	+++ comment "Don't forget to put the probe connectors for the tool length measurement."
	+++ pause
	+++ probeZMinus (V3 0 (d/2) 0) margin
	+++ comment "Remove the probe connectors"

strut' :: Double -> Double -> Operation IR
strut' d l = 
	probeStrut d l 5 (ProbeTool "02" 3 42)
	+++ zRepetition (-d) (Just 1) (saw_left d)

strut :: Double -> Double -> Operation IR
strut d l = 
	strut' (d+2) l
	+++ comment "Please rotate the strut to machine the other side"
	+++ pause
	+++ strut' d l 


op :: Operation IR
op = rotate 45 $ strut 8 30
		


main :: IO ()
--main = putStr . toString . runOperation $ op
main = interface . toGCode . runOperation $ op
