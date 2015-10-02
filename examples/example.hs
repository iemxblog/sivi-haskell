module Main (
	main
) where

import Sivi
import Linear


bigPocket = circularPocket 50 10 0.5

rectanglePlusCircle = chain 1 [
		rectangularPocket 15 10 10 0.5
		, translate (V3 12 0 0) (circularPocket 10 10 0.5)
	]

complexOp :: Operation IRTree
complexOp = translate (V3 0 0 (-10)) . circularRepetition 20 6 0 1 $ rectanglePlusCircle 
 	
op = complexOp	
		
main :: IO ()
main = putStr . (++"M2\n") . toString . flatten . runOperationWithDefaultParams $ op
--main = interface . toGCode . flatten . runOperationWithDefaultParams $ op
