module Main (
        main
) where

import Sivi
import Linear

strut' :: (Machine m, Backend w) => Double -> Double -> Operation m w ()
strut' d l = 
        chain 5 [
                probeHorizontalCylinderRight d l 5 (ProbeTool 3 42)
                , saw_left d d 1
        ]

strut :: (Machine m, Backend w) => Double -> Double -> Operation m w ()
strut d l = do
        strut' d (l+2)
        retract 30
        message "Please rotate the strut to machine the other side"
        strut' d l 

op :: (Machine m, Backend w) => Operation m w ()
op = strut 10 47.1
                
main :: IO ()
--main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters $ op
main = interface . getGCode defaultCuttingParameters $ op
