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

-- | Sawing operation. Cuts in the Y direction.
saw_left :: Double	-- ^ w : Width of the cut (tool radius compensation is automatic)
	-> Operation IR	-- ^ Resulting operation
saw_left w = do
		df <- getToolDiameter
		op1 <- approach $ V3 (-df/2) (-df/2) 0 
		op2 <- feed $ V3 (-df/2) (w+df/2) 0
		return (op1 ++ op2)

drill :: Operation IR
drill = approach (V3 0 0 0)
