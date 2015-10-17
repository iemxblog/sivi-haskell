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
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Operation.Probing.Base
import Sivi.Backend

-- | Probes a corner inside a rectangular pocket, in the North-East direction.
probeInnerCornerNE :: 	Backend a => Double	-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation a
probeInnerCornerNE margin probeTool = 
	withTool probeTool (
		message "Place the probe 5mm above the corner"
		+++ defCurPos (V3 0 0 5)
		+++ chain 5 [
			probeZMinus (V3 margin margin 0) margin
			, probeXPlus (V3 0 (-margin) (-5)) margin
			, probeYPlus (V3 (-margin) 0 (-5)) margin
		]
	)
	+++ message "Tool length measurement"
	+++ probeZMinus (V3 margin margin 0) margin
	+++ message "Finished probing"


-- | Probes a corner inside a rectangular pocket, in the North-West direction.
probeInnerCornerNW :: 	Backend a => Double 	-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation a
probeInnerCornerNW margin probeTool = rotate 90 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-West direction.
probeInnerCornerSW :: 	Backend a => Double 	-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation a
probeInnerCornerSW margin probeTool = rotate 180 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-East direction.
probeInnerCornerSE :: 	Backend a => Double	-- ^ margin : Probing margin
			-> Tool 		-- ^ probeTool : Tool used to probe the part
			-> Operation a
probeInnerCornerSE margin probeTool = rotate 270 $ probeInnerCornerNE margin probeTool
