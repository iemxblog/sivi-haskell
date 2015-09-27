{-|
Module		: Sivi.Operation.Probing.Base
Description	: Probing operations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Probing.Base (
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
probeHelper :: 	V3 Double		-- ^ dir : Direction
		-> Bool			-- ^ compFlag : True -> tool radius compensation, False -> no tool radius compensation
		-> V3 Double		-- ^ dst : Destination
		-> Double		-- ^ margin : Distance between initial probe position and destination
		-> Operation IRTree 	-- ^ Resulting Operation
probeHelper dir compFlag dst margin = 
	translate dst $ do
		td <- getToolDiameter
		let comp = case compFlag of
			True -> td/2
			False -> 0
		let initPos = (-1)*(margin+comp) *^ dir
		o1 <- approach_rapid initPos
		o2 <- probe ((margin+comp) *^ dir)
		o3 <- defCurPos (((-1) * comp) *^ dir)
		o4 <- rapid initPos
		return $ Node "" [o1, o2, o3, o4]
			
	
-- | Probes a part in the X direction, descending tool coordinate.
probeXMinus ::	V3 Double		-- ^ Point to probe
		 -> Double		-- ^ Margin (distance between point to probe and initial tool position)
		 -> Operation IRTree
probeXMinus = probeHelper (V3 (-1) 0 0) True

-- | Probes a part in the X direction, ascending tool coordinate.
probeXPlus :: 	V3 Double 		-- ^ Point to probe
		-> Double 		-- ^ Margin (distance between point to probe and initial tool position)
		-> Operation IRTree
probeXPlus = probeHelper (V3 1 0 0) True

-- | Probes a part in the Y direction, descending tool coordinate.
probeYMinus :: 	V3 Double 		-- ^ Point to probe
		-> Double 		-- ^ Margin (distance between point to probe and initial tool position)
		-> Operation IRTree
probeYMinus = probeHelper (V3 0 (-1) 0) True

-- | Probes a part in the Y direction, ascending tool coordinate.
probeYPlus :: 	V3 Double 		-- ^ Point to probe
		-> Double 		-- ^ Margin (distance between point to probe and initial tool position)
		-> Operation IRTree
probeYPlus = probeHelper (V3 0 1 0) True

-- | Probes a part in the Z direction, descending tool coordinate.
probeZMinus :: 	V3 Double		-- ^ Point to probe
		-> Double		-- ^ Margin (distance between point to probe and initial tool position) 
		-> Operation IRTree
probeZMinus = probeHelper (V3 0 0 (-1)) False

