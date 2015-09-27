module Main (
	main
) where

import Sivi
import Linear

strut' :: Double -> Double -> Operation IRTree
strut' d l = 
	chain 5 [
		probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
		, saw_left d d 1
	]

strut :: Double -> Double -> Operation IRTree
strut d l = 
	strut' d (l+2)
	+++ retract 30
	+++ comment "Please rotate the strut to machine the other side"
	+++ pause
	+++ strut' d l 

op :: Operation IRTree
op = strut 10 47.1
		
main :: IO ()
--main = putStr . (++"M2\n") . toString . flatten . runOperationWithDefaultParams $ op
main = interface . toGCode . flatten . runOperationWithDefaultParams $ op
