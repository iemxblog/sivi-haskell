module Operation.Base (
	Operation
	, noOp	
	, rapid
	, feed
	, plunge
	, retract
	, rapid_xy
	, approach
	, translate
	, (+++)
	, runOperation
	, pause
)
where

import IR
import Linear
import Control.Monad.State
import Control.Monad.Reader

-- | The Operation type
-- 
type Operation = ReaderT (V3 Double, Double, Double) (State (V3 Double)) [IR]

-- | Do-nothing operation
noOp :: Operation
noOp = lift $ return []


-- Variables :
-- dst : destination
-- or : origin
-- fr : feed rate
-- pr : plunge rate

-- | Rapid positioning
rapid :: V3 Double	-- ^ dst : Destination
	 -> Operation	-- Resulting operation
rapid dst = do
		(or, fr, pr) <- ask
		lift $ do 
			put (or+dst)
			return [Move (or+dst) Rapid]
-- | Linear interpolation (with the default feedrate)
feed :: V3 Double	-- ^ dst : Destination
	 -> Operation	-- Resulting operation
feed dst = do
		(or, fr, pr) <- ask
		lift $ do
			put (or+dst)
			return [Move (or+dst) (LinearInterpolation fr)]

-- | Linear interpolation (with the plunge feedrate)
plunge :: V3 Double	-- ^ dst : Destination
	 -> Operation	-- ^ Resulting operation
plunge dst = do
		(or, fr, pr) <- ask
		lift $ do
			put (or+dst)
			return [Move (or+dst) (LinearInterpolation pr)]

-- | Tool retraction
retract :: Double 	-- ^ z_safe : destination on the Z axis
	-> Operation	-- ^ Resulting operation
retract z_safe = do 
			(or, fr, pr) <- ask
			lift $ do
				V3 x y _ <- get
				let V3 _ _ zo = or
				put $ V3 x y (zo+z_safe)
				return [Move (V3 x y (zo+z_safe)) Rapid]

-- | Rapid in the XY plane (helper function for approach)
rapid_xy :: V3 Double	-- ^ dst : Destination
	 -> Operation	-- ^ Resulting operation
rapid_xy dst = do
			(or, fr, pr) <- ask
			lift $ do
				V3 _ _ z <- get
				let V3 xd yd _ = dst 
				let V3 xo yo _ = or
				put $ V3 (xo+xd) (yo+yd) z
				return [Move (V3 (xo+xd) (yo+yd) z) Rapid]
-- | Rapid in the xy plane + plunge to destination
approach :: V3 Double	-- ^ dst : Destination
	 -> Operation	-- ^ Resulting operation
approach dst = rapid_xy dst +++ plunge dst

-- | Translate an operation
translate :: V3 Double	-- ^ v : Translation vector
	-> Operation 	-- ^ o : Operation to translate
	-> Operation	-- Resulting operation
translate v o =	local (\(or, fr, pr)->(or+v, fr, pr)) o

-- | Chain two operations (without tool retraction between operations)
(+++) :: Operation 	-- ^ o1 : Operation 1
	-> Operation	-- ^ o2 : Operation 2
	-> Operation	-- ^ Operation 1 followed by operation 2
o1 +++ o2 = do 
		ir1 <- o1 
		ir2 <- o2
		return $ ir1 ++ ir2

-- | Pause operation, takes no arguments
pause :: Operation
pause = return [Pause]

-- | Runs an operation with default starting position, feed rate and plunge rate.
--
-- 	* Starting position : V3 0 0 0
--
-- 	* Feed rate : 100mm/min
--
-- 	* Plunge rate : 30 mm/min
runOperation :: Operation 	-- ^ o : The operation to run
		-> Program	-- ^ The resulting program in Intermediate Representation
runOperation o = evalState (runReaderT o (V3 0 0 0, 100, 30))  (V3 0 0 0)

