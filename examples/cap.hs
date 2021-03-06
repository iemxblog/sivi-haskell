module Main (
        main
) where

import Sivi
import Linear

d1 = 10 + 0.3
d2 = 14 + 0.4
d3 = 20 - 0.2
d4 = 26
h = 10
e1 = 1
e2 = 3
m = 1

cap :: (Machine m, Backend w) => Bool -> Operation m w ()
cap hole = chain 1 [
                circularPocket d2 (h-e1) 0.5
                , name "hole" $ if hole then translate (V3 0 0 (-h+e1)) (circularPocket d1 (e1+m) 0.5) else noOp
                , cylinderOuter d3 (h-e2)
                , cylinderOuter d4 (h+1)
        ]

caps :: (Machine m, Backend w) => Int -> Bool -> Operation m w ()
caps n hole = do
        message "Please place the tool above the center of the first cap"
        defCurPos (V3 0 0 0) 
        gridRepetition n 1 (d4+3) 0 1 (cap hole)

main :: IO ()
main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters {depthOfCut = -1} $ caps 1 True
--main = interface . getGCode defaultCuttingParameters {depthOfCut = -1} $ caps 1 True
