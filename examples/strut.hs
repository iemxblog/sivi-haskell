module Main (
	main
) where

import Sivi
import Linear

strut' :: Backend a => Double -> Double -> Operation a
strut' d l = 
	chain 5 [
		probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
		, saw_left d d 1
	]

strut :: Backend a => Double -> Double -> Operation a
strut d l = 
	strut' d (l+2)
	+++ retract 30
	+++ message "Please rotate the strut to machine the other side"
	+++ strut' d l 

op :: Backend a => Operation a
op = strut 10 47.1
		
main :: IO ()
--main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters $ op
main = interface . getGCode defaultCuttingParameters $ op
