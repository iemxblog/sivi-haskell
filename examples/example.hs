module Main (
	main
) where

import Sivi
import Linear hiding (rotate)


bigPocket = zRepetition (-10) Nothing (circularPocket 50 0.5)

rectanglePlusCircle = chain 1 . map (zRepetition (-10) Nothing) $ [
		rectangularPocket 15 10 0.5
		, translate (V3 12 0 0) (circularPocket 10 0.5)
	]

complexOp :: Operation IR
complexOp = translate (V3 0 0 (-10)) . circularRepetition 20 6 0 1 $ rectanglePlusCircle 
 	
--op = noOp +++ complexOp	
--op = translate (V3 0 10 0) ( rotate 45 rectanglePlusCircle)
op = rotate 45 rectanglePlusCircle
		
main :: IO ()
main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ op
--main = interface . toGCode . runOperationWithDefaultParams $ op
