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
probeHelper :: 	V3 Double	-- ^ dir : Direction
		-> Bool		-- ^ compFlag : True -> tool radius compensation, False -> no tool radius compensation
		-> V3 Double	-- ^ dst : Destination
		-> Double	-- ^ margin : Distance between initial probe position and destination
		-> Operation IR -- ^ Resulting Operation
probeHelper dir compFlag dst margin = 
	translate dst $ do
		td <- getToolDiameter
		let comp = case compFlag of
			True -> td/2
			False -> 0
		let initPos = ((-1)*(margin+comp) *^ dir)
		o1 <- approach_rapid initPos
		o2 <- probe ((margin+comp) *^ dir)
		o3 <- defCurPos (((-1) * comp) *^ dir)
		o4 <- rapid initPos
		return (o1 ++ o2 ++ o3 ++ o4)
			
	
-- | Probes a part in the X direction, from right to left.
probeXMinus :: 	V3 Double	
		 -> Double
		 -> Operation IR
probeXMinus = probeHelper (V3 (-1) 0 0) True

probeXPlus :: V3 Double -> Double -> Operation IR
probeXPlus = probeHelper (V3 1 0 0) True


probeYMinus :: V3 Double -> Double -> Operation IR
probeYMinus = probeHelper (V3 0 (-1) 0) True

probeYPlus :: V3 Double -> Double -> Operation IR
probeYPlus = probeHelper (V3 0 1 0) True

probeZMinus :: V3 Double -> Double -> Operation IR
probeZMinus = probeHelper (V3 0 0 (-1)) False


-- Ajouter DefCurPos dans applyMatrix #############################################################################################
