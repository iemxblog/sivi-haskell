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
	, retract
	, rapid_xy
	, approach
	, translate
	, (+++)
	, runOperation
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
				V3 x y _ <- get
				let V3 _ _ zo = or
				put $ V3 x y (zo+z_safe)
				return [Move (V3 x y (zo+z_safe)) Rapid]


rapid_xy :: V3 Double -> Operation
rapid_xy dst = do
			(or, fr, pr) <- ask
			lift $ do
				V3 _ _ z <- get
				let V3 xd yd _ = dst 
				let V3 xo yo _ = or
				put $ V3 (xo+xd) (yo+yd) z
				return [Move (V3 (xo+xd) (yo+yd) z) Rapid]

approach :: V3 Double -> Operation
approach dst = rapid_xy dst +++ plunge dst

translate :: V3 Double -> Operation -> Operation
translate v o =	local (\(or, fr, pr)->(or+v, fr, pr)) o

(+++) :: Operation -> Operation -> Operation
o1 +++ o2 = do 
		ir1 <- o1 
		ir2 <- o2
		return $ ir1 ++ ir2

next :: Double -> Operation -> Operation -> Operation
next z_safe o1 o2 = retract z_safe +++ o1 +++ retract z_safe +++ o2 

oplist :: Double -> [Operation] -> Operation
oplist _ [] = noOp
oplist z_safe (o:os) = next z_safe o (oplist z_safe os)


runOperation :: Operation -> Program
runOperation o = evalState (runReaderT o (V3 0 0 0, 100, 30))  (V3 0 0 0)

square = approach (V3 0 0 0) 
	+++ feed (V3 1 0 0)
	+++ feed (V3 1 1 0)
	+++ feed (V3 0 1 0)
	+++ feed (V3 0 0 0)


repetition :: [V3 Double] -> Double -> Operation -> Operation
repetition xs z_safe op = foldr (+++) noOp $ map (\v -> retract z_safe +++ translate v op) xs

circularRepetition :: Double -> Int -> Double -> Double -> Operation -> Operation
circularRepetition d n start_angle z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 (d/2*cos(f i)) (d/2*sin(f i)) 0 | i <- [0..n-1]]
		f a = fromIntegral(a)*2*pi/(fromIntegral n)+start_angle*pi/180

gridRepetition :: Int -> Int -> Double -> Double -> Double -> Operation -> Operation
gridRepetition nx ny space_x space_y z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 ((fromIntegral x)*space_x) ((fromIntegral y)*space_y) 0 | x <- [1..nx-1], y <- [1..ny-1]]

pause :: Operation
pause = return [Pause]
