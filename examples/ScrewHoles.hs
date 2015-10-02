module ScrewHoles (
	screwHoles
) where

import Sivi
import Linear

screwHoles :: Operation IR
screwHoles = circularRepetition (28*2) 2 0 1 $
		circularPocket 5 (10+0.2) 0.5
		+++ translate (V3 0 0 (-5)) (circularPocket 5 (5+0.5) 0.5) 
