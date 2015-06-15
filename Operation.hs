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
import Control.Monad.State
import Control.Monad.Reader

type Operation = ReaderT (V3 Double, Double, Double) (State (V3 Double)) [IR]

noOp :: Operation
noOp = lift $ return []


-- Variables :
-- dst : destination
-- or : origin
-- fr : feed rate
-- pr : plunge rate

rapid :: V3 Double -> Operation
rapid dst = do
		(or, fr, pr) <- ask
		lift $ do 
			put (or+dst)
			return [Move (or+dst) Rapid]

feed :: V3 Double -> Operation
feed dst = do
		(or, fr, pr) <- ask
		lift $ do
			put (or+dst)
			return [Move (or+dst) (LinearInterpolation fr)]

plunge :: V3 Double -> Operation
plunge dst = do
		(or, fr, pr) <- ask
		lift $ do
			put (or+dst)
			return [Move (or+dst) (LinearInterpolation pr)]

retract :: Double -> Operation
retract z_safe = do 
			(or, fr, pr) <- ask
			lift $ do
				p <- get
				put $ p + V3 0 0 z_safe
				return [Move (p + V3 0 0 z_safe) Rapid]

(+++) :: Operation -> Operation -> Operation
o1 +++ o2 = do 
		ir1 <- o1 
		ir2 <- o2 			-- z_safe ???????????????????????
		return $ ir1 ++ ir2

next :: Double -> Operation -> Operation -> Operation
next z_safe o1 o2 = retract z_safe +++ o1 +++ retract z_safe +++ o2 

oplist :: Double -> [Operation] -> Operation
oplist _ [] = noOp
oplist z_safe (o:os) = next z_safe o (oplist z_safe os)

--test = do
--	o1 <- rapid (V3 1 2 3)
--	o2 <- feed (V3 4 5 6) 
--	return (o1 +++ o2)

test2 = next 1 (feed (V3 1 0 0)) (feed (V3 0 1 0))

(+-+) :: (Double -> Operation) -> Operation -> Double -> Operation
do1 +-+ o2 = \z_safe -> do1 z_safe +++ retract z_safe +++ o2

square = atZ (plunge (V3 0 0 0)) 
	+-+ feed (V3 1 0 0) 
	+-+ feed (V3 1 1 0) 
	+-+ feed (V3 0 1 0) 
	$ 10 
--square = oplist 10 [feed (V3 1 0 0), feed (V3 1 1 0), feed (V3 0 1 0)]

atZ :: Operation -> Double -> Operation
atZ o = \z_safe -> retract z_safe +++ o



run :: Operation -> Program
run o = evalState (runReaderT o (V3 0 0 0, 100, 30))  (V3 0 0 0)
