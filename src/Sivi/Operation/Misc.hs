{-|
Module		: Sivi.Operation.Misc
Description	: Miscellaneous operations (saw, drill, ...)
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}

module Sivi.Operation.Misc (
	saw_left
	, drill
)
where
import Sivi.IR
import Linear
import Sivi.Operation.Base
import Sivi.Operation.Repetition

-- | Generates a single pass of a sawing operation. Cuts in the Y direction. Tool radius compensation is done on the left. P means "Pass".
saw_leftP :: Double		-- ^ w : Width of the cut (tool radius compensation is automatic)
	-> Operation IRTree	-- ^ Resulting operation
saw_leftP w = do
		df <- getToolDiameter
		op1 <- approach $ V3 (-df/2) (-df/2) 0 
		op2 <- feed $ V3 (-df/2) (w+df/2) 0
		return $ Node "" [op1, op2]

-- | Sawing operation. Cuts in the Y direction. Tool radius compensation is done on the left. 
saw_left :: Double		-- ^ w : Width of the cut (tool radius compensation is automatic)
	-> Double		-- ^ depth : Depth of the cut
	-> Double		-- ^ retraction : Altitude to go to between each pass
	-> Operation IRTree 	-- ^ Resulting operation
saw_left w depth retraction = zRepetition depth (Just retraction) (saw_leftP w)

-- | Generates a single pass of a drilling operation. P means "pass".
drillP :: Operation IRTree
drillP = approach (V3 0 0 0)

-- | Drilling operation. Modify the depth of cut with 'withDepthOfCut' if you want to drill faster.
drill :: Double 		-- ^ depth : Depth of the hole
	-> Double 		-- ^ retraction : Altitude to go to in order to evacuate the chips
	-> Operation IRTree	-- ^ Resulting operation
drill depth retraction = zRepetition depth (Just retraction) drillP
