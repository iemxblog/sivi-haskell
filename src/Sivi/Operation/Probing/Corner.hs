{-|
Module		: Sivi.Operation.Probing.Corner
Description	: Probing operations for corners
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Probing.Corner (
	probeInnerCornerNE
	, probeInnerCornerNW
	, probeInnerCornerSW
	, probeInnerCornerSE
) where

import Linear hiding (rotate)
import Sivi.IR.Base
import Sivi.Operation.Base
import Sivi.Operation.Probing.Base

-- | Probes a corner inside a rectangular pocket, in the North-East direction.
probeInnerCornerNE :: 	Double 			-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation IR
probeInnerCornerNE margin probeTool = 
	withTool probeTool (
		comment "Place the probe 5mm above the corner"
		+++ pause
		+++ defCurPos (V3 0 0 5)
		+++ chain 5 [
			probeZMinus (V3 margin margin 0) margin
			, probeXPlus (V3 0 (-margin) (-5)) margin
			, probeYPlus (V3 (-margin) 0 (-5)) margin
		]
	)
	+++ comment "Tool length measurement"
	+++ pause
	+++ probeZMinus (V3 margin margin 0) margin
	+++ comment "Finished probing"
	+++ pause


-- | Probes a corner inside a rectangular pocket, in the North-West direction.
probeInnerCornerNW :: 	Double 			-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation IR
probeInnerCornerNW margin probeTool = rotate 90 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-West direction.
probeInnerCornerSW :: 	Double 			-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation IR
probeInnerCornerSW margin probeTool = rotate 180 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-East direction.
probeInnerCornerSE :: 	Double 			-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation IR
probeInnerCornerSE margin probeTool = rotate 270 $ probeInnerCornerNE margin probeTool
