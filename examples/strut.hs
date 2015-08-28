module Main (
	main
) where

import Sivi
import Linear

strut' :: Double -> Double -> Operation IR
strut' d l = 
	chain 5 [
		probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
		, zRepetition (-d) (Just 1) (saw_left d)
	]

strut :: Double -> Double -> Operation IR
strut d l = 
	strut' d (l+2)
	+++ retract 30
	+++ comment "Please rotate the strut to machine the other side"
	+++ pause
	+++ strut' d l 

op :: Operation IR
op = strut 10 47.1
		
main :: IO ()
--main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ op
main = interface . toGCode . runOperationWithDefaultParams $ op
