{-|
Module		: Sivi.Operation.Base
Description	: Base operations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}

module Sivi.Operation.Base (
	Operation
	, getTransformation
	, getOrigin
	, getFeedRate
	, getPlungeRate
	, getProbeRate
	, getDepthOfCut
	, withTransformation
	, withFeedRate
	, withPlungeRate
	, withDepthOfCut
	, getCurrentPosition
	, putCurrentPosition
	, getTool
	, putTool
	, getToolDiameter
	, noOp	
	, rapid
	, feed
	, arc
	, arcNT
	, plunge
	, retract
	, rapid_xy
	, approach
	, approach_rapid
	, translate
	, rotate
	, (+++)
	, opsequence
	, chain
	, runOperationWithDefaultParams
	, pause
	, probe
	, defCurPos
	, comment
	, changeTool
	, withTool
)
where

import Sivi.IR
import Linear hiding (rotate)
import qualified Linear (rotate)
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.List

type Transformation = V3 Double -> V3 Double

-- | The Operation type.
-- Parameters are :
--
-- * Transformation : the current 'Transformation'
--
-- * Double : Feed rate
--
-- * Double : Plunge rate
--
-- * Double : Probe rate
--
-- * Double : Depth of cut
--
-- * Double (in the State Monad) : The current machine position
--
-- * Tool (in the State Monad) : The current tool
type Operation a = ReaderT (Transformation, Double, Double, Double, Double) (State (V3 Double, Tool)) a

getTransformation :: Operation Transformation
getTransformation = do
			(tr, _, _, _, _) <- ask
			return tr

-- | Returns the origin of an operation
getOrigin :: Operation (V3 Double)
getOrigin = do 
		tr <- getTransformation
		return $ tr (V3 0 0 0)

-- | Returns the current feed rate
getFeedRate :: Operation Double
getFeedRate = do
		(_, fr, _, _, _) <- ask
		return fr

-- | Returns the current plunge rate
getPlungeRate :: Operation Double
getPlungeRate = do
		(_, _, pr, _, _) <- ask
		return pr

-- | Returns the current probe rate
getProbeRate :: Operation Double
getProbeRate = do
		(_, _, _, pbr, _) <- ask
		return pbr


-- | Returns the current depth of cut
getDepthOfCut :: Operation Double
getDepthOfCut = do
		(_, _, _, _, dc) <- ask
		return dc

-- | Calls an operation with the specified transformation
withTransformation :: 	Transformation 		-- ^ ntr : The new transformation
			-> Operation IR 	-- ^ The operation to call with the specified transformation
			-> Operation IR		-- ^ The resulting operation
withTransformation ntr = local (\(tr, fr, pr, pbr, dc) -> (tr . ntr, fr, pr, pbr, dc))

-- | Calls an operation with the specified feed rate
withFeedRate :: Double 			-- ^ nfr : The new feed rate
		-> Operation IR 	-- ^ The operation to call with the new feed rate
		-> Operation IR		-- ^ The resulting operation
withFeedRate nfr = local (\(tr, _, pr, pbr, dc) -> (tr, nfr, pr, pbr, dc))

-- | Calls an operation with the specified plunge rate
withPlungeRate :: Double 		-- ^ npr : The new plunge rate
		-> Operation IR 	-- ^ The operation to call with the new plunge rate
		-> Operation IR		-- ^ The resulting operation
withPlungeRate npr = local (\(tr, fr, _, pbr, dc) -> (tr, fr, npr, pbr, dc))

-- | Calls an operation with the specified depth of cut
withDepthOfCut :: Double 		-- ^ ndc : The new depth of cut
		-> Operation IR 	-- ^ The operation to call with the new depth of cut
		-> Operation IR		-- ^ The resulting operation
withDepthOfCut ndc = local (\(tr, fr, pr, pbr, _) -> (tr, fr, pr, pbr, ndc))
	
-- | Returns the machine's current position (from the State monad)
getCurrentPosition :: Operation (V3 Double)
getCurrentPosition = liftM fst (lift get)

-- | Sets the current position
putCurrentPosition :: V3 Double -> Operation ()
putCurrentPosition cp = do
				(_, t) <- get
				put (cp, t)

-- | Returns the current tool
getTool :: Operation Tool
getTool = liftM snd (lift get)

-- | Sets the current tool
putTool :: Tool -> Operation ()
putTool t = do
		(cp, _) <- get
		put (cp, t)

-- | Returns the current tool's diameter
getToolDiameter :: Operation Double
getToolDiameter = do
			tool <- getTool
			return $ diameter tool

-- | Do-nothing operation
noOp :: Operation IR
noOp = lift $ return []

-- | Helper function to avoid duplicate code. Sets the current position in the State monad, and returns a 'Move'. The current transformation is NOT applied to the parameters.
move :: V3 Double -> MoveParams -> Operation IR
move dst params = putCurrentPosition dst >> return [Move dst params] 

-- Variables :
-- dst : destination
-- tr : transformation
-- fr : feed rate
-- pr : plunge rate
-- dc : depth of cut

-- | Rapid positioning
rapid :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- Resulting operation
rapid dst = do
		tr <- getTransformation
		move (tr dst) Rapid

-- | Rapid positioning, but does not apply the current transformation. Not meant to be exported, only used as an internal helper function. (NT means "no transformation")
rapidNT :: V3 Double		-- ^ dst : Destination
	-> Operation IR		-- ^ Resulting operation
rapidNT dst = move dst Rapid

-- | Linear interpolation (with the default feedrate)
feed :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- Resulting operation
feed dst = do
		tr <- getTransformation
		fr <- getFeedRate	
		move (tr dst) (LinearInterpolation fr)

-- | Circular interpolation
arc :: ArcDirection 		-- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
	-> V3 Double		-- ^ center : The center of the arc (relative to the origin of the operation)
	-> V3 Double		-- ^ dst : The destination point
	-> Operation IR		-- ^ Resulting operation
arc dir center dst = do
			tr <- getTransformation
			fr <- getFeedRate
			move (tr dst) Arc { direction=dir, center=(tr center), feedRate=fr}

-- | Circular interpolation, but does not apply the current transformation. Exported only for 'circleFromHere'. Only used as an internal helper function. (NT means "no transformation")
arcNT :: ArcDirection 		-- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
	-> V3 Double		-- ^ center : The center of the arc (relative to the origin of the operation)
	-> V3 Double		-- ^ dst : The destination point
	-> Operation IR		-- ^ Resulting operation
arcNT dir center dst = do
			fr <- getFeedRate
			move dst Arc { direction=dir, center=center, feedRate=fr}


-- | Linear interpolation (with the plunge feedrate)
plunge :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- ^ Resulting operation
plunge dst = do
		tr <- getTransformation
		pr <- getPlungeRate
		move (tr dst) (LinearInterpolation pr)

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
			tr <- getTransformation
			V3 _ _ z <- getCurrentPosition	
			let V3 xd yd _ = tr dst 
			move (V3 xd yd z) Rapid

-- | Rapid in the xy plane + rapid plunge with margin (2 * depth of cut above destination) + plunge (with plunge rate) to destination
approach :: V3 Double		-- ^ dst : Destination
	 -> Operation IR	-- ^ Resulting operation
approach dst = do
	tr <- getTransformation
	let V3 _ _ zd = tr dst
	V3 _ _ z <- getCurrentPosition	
	dc <- getDepthOfCut
	op1 <- rapid_xy dst 
	op2 <- if (z-zd) > 2*(abs dc) then rapidNT (tr dst + V3 0 0 (2* (abs dc))) else noOp
	op3 <- plunge dst
	return (op1 ++ op2 ++ op3)

-- | Same as approach, but plunge with rapid move only
approach_rapid :: V3 Double	-- ^ dst : Destination
		-> Operation IR -- ^ Resulting operation
approach_rapid dst = rapid_xy dst +++ rapid dst

-- | Translate an operation
translate :: V3 Double		-- ^ v : Translation vector
	-> Operation IR		-- ^ o : Operation to translate
	-> Operation IR		-- Resulting operation
translate v = withTransformation (+v)

-- | Rotates an operation in the XY plane.
rotate :: Double		-- ^ a : angle (in degrees)
	-> Operation IR		-- ^ Operation to rotate
	-> Operation IR		-- ^ Resulting operation
rotate a = withTransformation (Linear.rotate (axisAngle (V3 0 0 1) ar))
	where ar = a * pi / 180

-- | Chain two operations (without tool retraction between operations)
(+++) :: Operation IR	-- ^ o1 : Operation 1
	-> Operation IR	-- ^ o2 : Operation 2
	-> Operation IR	-- ^ Operation 1 followed by operation 2
o1 +++ o2 = do 
		ir1 <- o1 
		ir2 <- o2
		return $ ir1 ++ ir2

-- | Chains a list of operations.
opsequence :: [Operation IR] -> Operation IR
opsequence = foldr (+++) noOp

-- | Chains a list of operations, and intersperses tool retractions between them.
chain :: Double 		-- ^ zSafe : The altitude of tool retractions
	-> [Operation IR] 	-- ^ The list of operations
	-> Operation IR		-- ^ The resulting operation
chain zSafe = opsequence . intersperse (retract zSafe) 

-- | Pause operation, takes no arguments
pause :: Operation IR
pause = return [Pause]

-- | Basic probing operation.
probe :: V3 Double 		-- ^ dst : The destination of the probing move
	-> Operation IR		-- ^ The resulting operation.
probe dst = do
		tr <- getTransformation
		pbr <- getProbeRate
		move (tr dst) (Probe pbr)	

defCurPos :: V3 Double -> Operation IR
defCurPos p = do
		tr <- getTransformation
		putCurrentPosition (tr p) 
		return [DefCurPos (tr p)]

comment :: String -> Operation IR
comment s = return [Comment s]

changeTool :: Tool -> Operation IR
changeTool t = retract 30
		+++ comment ("Please place the tool " ++ show t ++ " in the spindle.")
		+++ pause
		<* putTool t

-- | Do an operation with a temporary tool.
withTool :: 	Tool 		-- ^ t : The tool to use for the operation
		-> Operation IR -- ^ op : The operation to run with the given tool.
		-> Operation IR -- ^ The resulting operation.
withTool t op = getTool >>= (\mt -> changeTool t +++ op +++ changeTool mt)
	
	
-- | Runs an operation with default starting position, feed rate, plunge rate and tool.
--
-- 	* Starting transformation : id
--
-- 	* Feed rate : 100mm/min
--
-- 	* Plunge rate : 30 mm/min
--
-- 	* Probe rate : 10 mm/min
--
-- 	* Depth of cut : -0.5 mm
--
--	* Tool : EndMill : diameter=3 length=42
runOperationWithDefaultParams :: Operation IR	-- ^ o : The operation to run
		-> IR		-- ^ The resulting program in Intermediate Representation
runOperationWithDefaultParams o = evalState (runReaderT o (id, 100, 30, 10, (-0.5)))  (V3 0 0 0, EndMill {diameter=3, len=42})

