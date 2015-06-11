{-|
Module		: Operation
Description	: Composable operations to build a CNC program
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Operation(
) where
import IR
import Linear

type Operation = V3 Double -> [IR]

rapid :: V3 Double -> Operation
rapid p = \sp -> [Move (sp+p) Rapid]

feed :: V3 Double -> Double -> Operation
feed p f = \sp -> [Move (sp+p) (LinearInterpolation f)]


(+++) :: Operation -> Operation -> Operation
o1 +++ o2 = \sp -> o1 sp ++ o2 sp

-- Operation a = Floating b => V3 b -> a
-- ou 
-- Operation = Floating b => V3 b -> [IR]

test = do
	o1 <- rapid (V3 1 2 3)
	o2 <- feed (V3 4 5 6) 100.0
	return (o1 ++ o2)
