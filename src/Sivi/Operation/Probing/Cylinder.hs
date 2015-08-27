{-|
Module		: Sivi.Operation.Probing.Cylinder
Description	: Probing operations for cylinders
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Probing.Cylinder (
	probeOuterCylinder
) where

import Linear
import Sivi.IR.Base
import Sivi.Operation.Base
import Sivi.Operation.Probing.Base

probeOuterCylinder :: Double -> Double -> Tool -> Operation IR
probeOuterCylinder d margin probeTool = 
	withTool probeTool (
		comment "Place the probe 5mm above the center of the cylinder"
		+++ pause
		+++ defCurPos (V3 0 0 5)
		+++ chain 5 [
			probeZMinus (V3 (d/4) 0 0) margin
			, probeXPlus (V3 (-d/2) 0 (-5)) margin
			, probeYPlus (V3 0 (-d/2) (-5)) margin
		]
	)
	+++ comment "Tool length measurement"
	+++ pause
	+++ probeZMinus (V3 (d/4) 0 0) margin
	+++ comment "Finished probing."
	+++ pause
