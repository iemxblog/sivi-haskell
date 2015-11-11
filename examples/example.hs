module Main (
	main
) where

import Sivi
import Linear

bigPocket :: Backend a => Operation a
bigPocket = circularPocket 50 10 0.5

rectanglePlusCircle :: Backend a => Operation a
rectanglePlusCircle = chain 1 [
		rectangularPocket 15 10 10 0.5
		, translate (V3 12 0 0) (circularPocket 10 10 0.5)
	]

complexOp :: Backend a => Operation a
complexOp = translate (V3 0 0 (-10)) . circularRepetition 20 6 0 1 $ rectanglePlusCircle 
 
op :: Backend a => Operation a	
op = complexOp	


raw = Translate (V3 0 0 (-20)) $ Cylinder 20 50 50 False

main :: IO ()
main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters $ op
--main = interface . getGCode defaultCuttingParameters $ op
--main = putStr $ simulation raw defaultCuttingParameters op
