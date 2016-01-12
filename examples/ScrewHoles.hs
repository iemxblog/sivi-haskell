module ScrewHoles (
        screwHoles
) where

import Sivi
import Linear

screwHoles :: (Machine m, Backend w) => Operation m w ()
screwHoles = circularRepetition (28*2) 2 0 1 $ do
                circularPocket 5 (10+0.2) 0.5
                translate (V3 0 0 (-5)) (circularPocket 5 (5+0.5) 0.5) 
