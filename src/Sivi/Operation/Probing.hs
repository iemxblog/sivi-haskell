{-|
Module		: Sivi.Operation.Probing
Description	: Base operations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Probing (
	probeXMinus
	, probeXPlus
	, probeYMinus
	, probeYPlus
	, probeZMinus
) where

import Linear
import Sivi.Operation.Base
import Sivi.IR.Base

-- | To be used with translate to define the starting point of the probe move.
probeHelper :: 	V3 Double	-- ^ Direction
		-> V3 Double	-- ^ Destination
		-> Double	-- ^ margin : Distance between initial probe position and destination
		-> Operation IR -- ^ Resulting Operation
probeHelper dir dst margin = 
	translate dst $ do
		td <- getToolDiameter
		let initPos = ((-1)*(margin+td/2) *^ dir)
		o1 <- approach_rapid initPos
		o2 <- probe ((margin+td/2) *^ dir)
		o3 <- defCurPos (((-1) * td/2) *^ dir)
		o4 <- rapid initPos
		return (o1 ++ o2 ++ o3 ++ o4)
			
	
-- | Probes a part in the X direction, from right to left.
probeXMinus :: 	V3 Double	
		 -> Double
		 -> Operation IR
probeXMinus = probeHelper (V3 (-1) 0 0)

probeXPlus :: V3 Double -> Double -> Operation IR
probeXPlus = probeHelper (V3 1 0 0)


probeYMinus :: V3 Double -> Double -> Operation IR
probeYMinus = probeHelper (V3 0 (-1) 0)

probeYPlus :: V3 Double -> Double -> Operation IR
probeYPlus = probeHelper (V3 0 1 0)

probeZMinus :: V3 Double -> Double -> Operation IR
probeZMinus = probeHelper (V3 0 0 (-1))

--probeCylinder :: Double -> Double -> Tool -> Operation IR
--probeCylinder d l probe =
--	do {
--		t <- getTool;
--		changeTool probe;
--		td <- getToolDiameter;
--		(comment "Place the probe 5mm above the right side of the cylinder, centered on the axis of the cylinder."
--		+++ pause
--		+++ translate (V3 0 0 margin) (probeZMinus (2*margin) 0)
--		+++ translate (V3 (margin+td/2) 0 (-d/2-td/2)) (probeXMinus (2*margin) (l+td/2))
--		+++ retract 5
--		+++ translate (V3 0 (-margin-d/2-td/2) (-d/2-td/2)) (probeYPlus (2*margin) (-d/2-td/2))
--		+++ retract 5
--		+++ changeTool t
--		+++ translate (V3 0 0 margin) (probeZMinus (2*margin) 0)
--		)
--	}
--	where margin = 10




-- Ajouter DefCurPos dans applyMatrix #############################################################################################