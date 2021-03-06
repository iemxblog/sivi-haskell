module Main (
        main
) where

import Sivi
import Linear

coupling :: (Machine m, Backend w) => Operation m w ()
coupling =      chain 5 [
                        probeOuterCylinder 20 5 (ProbeTool 3 42)
                        , cylinderInner 4.5 7
                ]

main :: IO ()
--main = putStr . (++"M2\n") . show . getGCode defaultCuttingParameters $ coupling
main = interface . getGCode defaultCuttingParameters $ coupling
