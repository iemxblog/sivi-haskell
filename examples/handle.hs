module Main (
        main
) where

import Sivi
import Linear


cut :: (Machine m, Backend w) => Double -> Double -> Operation m w ()
cut d l = 
        chain 5 [
                probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
                , saw_left d d 1
        ]

drillings :: (Machine m, Backend w) => Double -> Double -> Double -> Double -> Operation m w ()
drillings d l d1 d2 = 
    withTool (EndMill 2 42) $
        chain 5 [
            probeZMinus (V3 d1 (d/2) 0) 5
            , message "Start the spindle"
            , translate (V3 d1 (d/2) 0) (drill (d+1) 10)
            , translate (V3 (l-d2) (d/2) 0) (drill (d+1) 10)
        ]

axialDrilling :: (Machine m, Backend w) => Double -> Double -> Double -> Operation m w ()
axialDrilling dc dd l = 
    chain 5 [
        probeOuterCylinder dc 5 (ProbeTool 3 42)
        , circularPocket dd l 0.5
    ]

handle :: (Machine m, Backend w) => Operation m w ()
handle = do
    let l = 12
    let d = 10
    let d1 = 7.2
    let d2 = 3
    chain 5 [
        do
            cut d (l+2)
            retract 30
            message "Please rotate the part to cut the other side"
            cut d l
        , drillings d l d1 d2 ]
    axialDrilling d 4.5 l

main :: IO ()
--main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters $ handle
main = interface . getGCode defaultCuttingParameters $ handle
