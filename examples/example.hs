module Main (
        main
) where

import Sivi
import Linear

op :: (Machine m, Backend w) => Operation m w ()
op = circularPocket 50 10 0.5 |.| 
        (circularRepetition 20 6 0 1 (
            rectangularPocket 15 10 10 0.5 |>| circularPocket 10 10 0.5))

raw = Translate (V3 0 0 (-20)) $ Cylinder 30 55 55 False

main :: IO ()
main = putStr . (++"M2\n") . show . getGCode MF70 defaultCuttingParameters $ op
--main = interface . getGCode MF70 defaultCuttingParameters $ op
--main = putStr $ simulation raw MF70 defaultCuttingParameters op
