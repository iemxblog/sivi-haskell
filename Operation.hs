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
	Operation
	, rapid
	, feed
	, plunge
) where
import IR
import Linear

type Operation = (V3 Double, Double,  Double) -> [IR]

noOp :: Operation
noOp = \_ -> []

rapid :: V3 Double -> Operation
rapid p = \(sp, fr, pr) -> [Move (sp+p) Rapid]

feed :: V3 Double -> Operation
feed p = \(sp, fr, pr) -> [Move (sp+p) (LinearInterpolation fr)]

plunge :: V3 Double -> Operation
plunge p = \(sp, fr, pr) -> [Move (sp+p) (LinearInterpolation pr)]

retract :: Double -> Operation
retract z_safe = rapid (V3 0 0 z_safe)

(+++) :: Operation -> Operation -> Operation
o1 +++ o2 = \(sp, fr, pr)-> o1 (sp, fr, pr) ++ o2 (sp, fr, pr)			-- z_safe ???????????????????????

next :: Double -> Operation -> Operation -> Operation
next z_safe o1 o2 = retract z_safe +++ o1 +++ retract z_safe +++ o2 

oplist :: Double -> [Operation] -> Operation
oplist _ [] = noOp
oplist z_safe (o:os) = next z_safe o (oplist z_safe os)

test = do
	o1 <- rapid (V3 1 2 3)
	o2 <- feed (V3 4 5 6) 
	return (o1 ++ o2)


test2 = next 1 (feed (V3 1 0 0)) (feed (V3 0 1 0))

(+-+) :: (Double -> Operation) -> Operation -> Double -> Operation
do1 +-+ o2 = \z_safe -> do1 z_safe +++ retract z_safe +++ o2

square = atZ (feed (V3 1 0 0)) +-+ feed (V3 1 1 0) +-+ feed (V3 0 1 0) $ 10 
--square = oplist 10 [feed (V3 1 0 0), feed (V3 1 1 0), feed (V3 0 1 0)]

atZ :: Operation -> Double -> Operation
atZ o = \z_safe -> retract z_safe +++ o



run :: Operation -> Program
run o = o (V3 0 0 0, 100, 30)
