module Main (
        main
) where

import Sivi
import Linear

bigPocket :: (Machine m, Backend w) => Operation m w ()
bigPocket = circularPocket 50 10 0.5

rectanglePlusCircle :: (Machine m, Backend w) => Operation m w ()
rectanglePlusCircle = chain 1 [
                rectangularPocket 15 10 10 0.5
                , translate (V3 12 0 0) (circularPocket 10 10 0.5)
        ]

complexOp :: (Machine m, Backend w) => Operation m w ()
complexOp = translate (V3 0 0 (-10)) . circularRepetition 20 6 0 1 $ rectanglePlusCircle 
 
op :: (Machine m, Backend w) => Operation m w ()
op = complexOp  


raw = Translate (V3 0 0 (-20)) $ Cylinder 20 50 50 False

main :: IO ()
main = putStr . (++"M2\n") . show . getGCode MF70 defaultCuttingParameters $ op
--main = interface . getGCode MF70 defaultCuttingParameters $ op
--main = putStr $ simulation raw MF70 defaultCuttingParameters op
