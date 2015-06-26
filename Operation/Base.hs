module Operation.Base (
	Operation
	, getOrigin
	, getFeedRate
	, getPlungeRate
	, getTool
	, getToolDiameter
	, getCurrentPosition
	, noOp	
	, move
	, rapid
	, feed
	, arc
	, plunge
	, retract
	, rapid_xy
	, approach
	, translate
	, (+++)
	, opsequence
	, runOperation
	, pause
)
where

import IR
import Linear
import Control.Monad.State
import Control.Monad.Reader

-- | The Operation type.
-- Parameters are :
--
-- * V3 Double : the operation's origin (for example, 'translate' moves the origin)
--
-- * Double : Feed rate
--
-- * Double : Plunge rate
--
-- * Tool : The current tool
--
-- * Double (in the State Monad) : the current machine position
type Operation a = ReaderT (V3 Double, Double, Double, Tool) (State (V3 Double)) a

-- | Returns the origin of an operation
getOrigin :: Operation (V3 Double)
getOrigin = do 
		(or, _, _, _) <- ask
		return or

-- | Returns the current feed rate
getFeedRate :: Operation Double
getFeedRate = do
		(_, fr, _, _) <- ask
		return fr

-- | Returns the current plunge rate
getPlungeRate :: Operation Double
getPlungeRate = do
		(_, _, pr, _) <- ask
		return pr

-- | Returns the current tool
getTool :: Operation Tool
getTool = do
		(_, _, _, tool) <- ask
		return tool

-- | Returns the current tool's diameter
getToolDiameter :: Operation Double
getToolDiameter = do
			tool <- getTool
			return $ diameter tool
		
-- | Returns the machine's current position (from the State monad)
getCurrentPosition :: Operation (V3 Double)
getCurrentPosition = lift get

-- | Do-nothing operation
noOp :: Operation IR
noOp = lift $ return []

-- | Helper function to avoid duplicate code
move :: V3 Double -> MoveParams -> Operation IR
move dst params = lift $ put dst >> return [Move dst params] 

-- Variables :
-- dst : destination
-- or : origin
-- fr : feed rate
-- pr : plunge rate

-- | Rapid positioning
rapid :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- Resulting operation
rapid dst = do
		or <- getOrigin
		move (or+dst) Rapid

-- | Linear interpolation (with the default feedrate)
feed :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- Resulting operation
feed dst = do
		or <- getOrigin
		fr <- getFeedRate	
		move (or+dst) (LinearInterpolation fr)

-- | Circular interpolation
arc :: ArcDirection 		-- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
	-> V3 Double		-- ^ center : The center of the arc (relative to the origin of the operation)
	-> V3 Double		-- ^ dst : The destination point
	-> Operation IR		-- ^ Resulting operation
arc dir center dst = do
			or <- getOrigin
			fr <- getFeedRate
			move (or+dst) (Arc { direction=dir, center=(or+center), feedRate=fr})	

-- | Linear interpolation (with the plunge feedrate)
plunge :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- ^ Resulting operation
plunge dst = do
		or <- getOrigin
		pr <- getPlungeRate
		move (or+dst) (LinearInterpolation pr)

-- | Tool retraction
retract :: Double 		-- ^ z_safe : destination on the Z axis
	-> Operation IR		-- ^ Resulting operation
retract z_safe = do 
			V3 _ _ zo <- getOrigin
			V3 x y _ <- getCurrentPosition
			move (V3 x y (zo+z_safe)) Rapid

-- | Rapid in the XY plane (helper function for 'approach')
rapid_xy :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- ^ Resulting operation
rapid_xy dst = do
			V3 xo yo _ <- getOrigin
			V3 _ _ z <- getCurrentPosition	
			let V3 xd yd _ = dst 
			move (V3 (xo+xd) (yo+yd) z) Rapid

-- | Rapid in the xy plane + plunge to destination
approach :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- ^ Resulting operation
approach dst = rapid_xy dst +++ plunge dst

-- | Translate an operation
translate :: V3 Double		-- ^ v : Translation vector
	-> Operation IR		-- ^ o : Operation to translate
	-> Operation IR		-- Resulting operation
translate v o =	local (\(or, fr, pr, tool)->(or+v, fr, pr, tool)) o

-- | Chain two operations (without tool retraction between operations)
(+++) :: Operation IR	-- ^ o1 : Operation 1
	-> Operation IR	-- ^ o2 : Operation 2
	-> Operation IR	-- ^ Operation 1 followed by operation 2
o1 +++ o2 = do 
		ir1 <- o1 
		ir2 <- o2
		return $ ir1 ++ ir2

opsequence :: [Operation IR] -> Operation IR
opsequence = foldr (+++) noOp

-- | Pause operation, takes no arguments
pause :: Operation IR
pause = return [Pause]

-- | Runs an operation with default starting position, feed rate, plunge rate and tool.
--
-- 	* Starting position : V3 0 0 0
--
-- 	* Feed rate : 100mm/min
--
-- 	* Plunge rate : 30 mm/min
--
--	* Tool : EndMill : name="01" diameter=3 length=42
runOperation :: Operation IR	-- ^ o : The operation to run
		-> IR		-- ^ The resulting program in Intermediate Representation
runOperation o = evalState (runReaderT o (V3 0 0 0, 100, 30, EndMill {name="01", diameter=3, len=42}))  (V3 0 0 0)

