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
	getTransformation
	, getOrigin
	, getFeedRate
	, getPlungeRate
	, getProbeRate
	, getDepthOfCut
	, withTransformation
	, withFeedRate
	, withPlungeRate
	, withProbeRate
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
	, symmetryX
	, symmetryY
	, (+++)
	, opsequence
	, chain
	, runOperation
	, runOperationWithDefaultParams
	, pause
	, probe
	, defCurPos
	, comment
	, changeTool
	, withTool
	, name
	, message
)
where

import Sivi.Backend
import Sivi.Operation.Types
import Linear hiding (rotate)
import qualified Linear (rotate)
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.List
import Data.Monoid

-- | Returns the current transformation
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
			-> Operation a	 	-- ^ The operation to call with the specified transformation
			-> Operation a		-- ^ The resulting operation
withTransformation ntr = local (\(tr, fr, pr, pbr, dc) -> (tr . ntr, fr, pr, pbr, dc))

-- | Calls an operation with the specified feed rate
withFeedRate :: Double 			-- ^ nfr : The new feed rate
		-> Operation a 		-- ^ The operation to call with the new feed rate
		-> Operation a		-- ^ The resulting operation
withFeedRate nfr = local (\(tr, _, pr, pbr, dc) -> (tr, nfr, pr, pbr, dc))

-- | Calls an operation with the specified plunge rate
withPlungeRate :: Double 		-- ^ npr : The new plunge rate
		-> Operation a		-- ^ The operation to call with the new plunge rate
		-> Operation a		-- ^ The resulting operation
withPlungeRate npr = local (\(tr, fr, _, pbr, dc) -> (tr, fr, npr, pbr, dc))

-- | Calls an operation with the specified probe rate
withProbeRate :: Double 		-- ^ npbr : The new probe rate
		-> Operation a		-- ^ The operation to call with the new probe rate
		-> Operation a		-- ^ The resulting operation
withProbeRate npbr = local (\(tr, fr, pr, _, dc) -> (tr, fr, pr, npbr, dc))

-- | Calls an operation with the specified depth of cut
withDepthOfCut :: Double 		-- ^ ndc : The new depth of cut
		-> Operation a	 	-- ^ The operation to call with the new depth of cut
		-> Operation a		-- ^ The resulting operation
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
noOp :: Monoid m => Operation m
noOp = lift $ return mempty

-- Variables :
-- dst : destination
-- tr : transformation
-- fr : feed rate
-- pr : plunge rate
-- dc : depth of cut

-- | Helper function
move :: Backend a => V3 Double -> (V3 Double -> Operation a) -> Operation a
move dst op = op dst <* putCurrentPosition dst

-- | Rapid positioning
rapid :: Backend a => V3 Double		-- ^ dst : Destination
	 -> Operation a			-- Resulting operation
rapid dst = do
		tr <- getTransformation
		move (tr dst) bRapid

-- | Rapid positioning, but does not apply the current transformation. Not meant to be exported, only used as an internal helper function. (NT means "no transformation")
rapidNT :: Backend a => V3 Double	-- ^ dst : Destination
	-> Operation a			-- ^ Resulting operation
rapidNT dst = move dst bRapid

-- | Linear interpolation (with the default feedrate)
feed :: Backend a => V3 Double		-- ^ dst : Destination
	 -> Operation a 		-- Resulting operation
feed dst = do
		tr <- getTransformation
		fr <- getFeedRate	
		move (tr dst) (bFeed fr)

-- | Circular interpolation
arc :: Backend a => ArcDirection 	-- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
	-> V3 Double			-- ^ center : The center of the arc (relative to the origin of the operation)
	-> V3 Double			-- ^ dst : The destination point
	-> Operation a			-- ^ Resulting operation
arc dir center dst = do
			tr <- getTransformation
			fr <- getFeedRate
			move (tr dst) (bArc fr dir center)

-- | Circular interpolation, but does not apply the current transformation. Exported only for 'circleFromHere'. Only used as an internal helper function. (NT means "no transformation")
arcNT :: Backend a => ArcDirection 	-- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
	-> V3 Double			-- ^ center : The center of the arc (relative to the origin of the operation)
	-> V3 Double			-- ^ dst : The destination point
	-> Operation a 			-- ^ Resulting operation
arcNT dir center dst = do
			fr <- getFeedRate
			move dst (bArc fr dir center)

-- | Linear interpolation (with the plunge feedrate)
plunge :: Backend a => V3 Double	-- ^ dst : Destination
	 -> Operation a			-- ^ Resulting operation
plunge dst = do
		tr <- getTransformation
		pr <- getPlungeRate
		move (tr dst) (bFeed pr)

-- | Tool retraction
retract :: Backend a => Double 		-- ^ z_safe : destination on the Z axis
	-> Operation a 			-- ^ Resulting operation
retract z_safe = do 
			V3 _ _ zo <- getOrigin
			V3 x y _ <- getCurrentPosition
			rapidNT (V3 x y (zo+z_safe))

-- | Rapid in the XY plane (helper function for 'approach')
rapid_xy :: Backend a => V3 Double	-- ^ dst : Destination
	 -> Operation a			-- ^ Resulting operation
rapid_xy dst = do
			tr <- getTransformation
			V3 _ _ z <- getCurrentPosition	
			let V3 xd yd _ = tr dst 
			rapidNT (V3 xd yd z)

-- | Rapid in the xy plane + rapid plunge with margin (2 * depth of cut above destination) + plunge (with plunge rate) to destination
approach :: Backend a => V3 Double	-- ^ dst : Destination
	 -> Operation a			-- ^ Resulting operation
approach dst = do
	tr <- getTransformation
	let V3 _ _ zd = tr dst
	V3 _ _ z <- getCurrentPosition	
	dc <- getDepthOfCut
	op1 <- rapid_xy dst 
	op2 <- if (z-zd) > 2 * abs dc then rapidNT (tr dst + V3 0 0 (2 * abs dc)) else noOp
	op3 <- plunge dst
	return $ mconcat [op1, op2, op3]

-- | Same as approach, but plunge with rapid move only
approach_rapid :: Backend a => V3 Double	-- ^ dst : Destination
		-> Operation a			-- ^ Resulting operation
approach_rapid dst = rapid_xy dst +++ rapid dst

-- | Translate an operation
translate :: V3 Double		-- ^ v : Translation vector
	-> Operation a		-- ^ o : Operation to translate
	-> Operation a		-- Resulting operation
translate v = withTransformation (+v)

-- | Rotates an operation in the XY plane.
rotate :: Double		-- ^ a : angle (in degrees)
	-> Operation a		-- ^ Operation to rotate
	-> Operation a		-- ^ Resulting operation
rotate a = withTransformation (Linear.rotate (axisAngle (V3 0 0 1) ar))
	where ar = a * pi / 180

-- | Symmetry about the X axis.
symmetryX :: 	Operation a
		-> Operation a
symmetryX = withTransformation (\(V3 x y z) -> V3 x (-y) z)

-- | Symmetry about the Y axis.
symmetryY :: 	Operation a
		-> Operation a
symmetryY = withTransformation (\(V3 x y z) -> V3 (-x) y z)

-- | Chain two operations (without tool retraction between operations)
(+++) :: Monoid m => Operation m	-- ^ o1 : Operation 1
	-> Operation m			-- ^ o2 : Operation 2
	-> Operation m			-- ^ Operation 1 followed by operation 2
o1 +++ o2 = do 
		m1 <- o1 
		m2 <- o2
		return $ m1 `mappend` m2

-- | Chains a list of operations.
opsequence :: Monoid m => [Operation m] -> Operation m
opsequence = foldr (+++) noOp

-- | Chains a list of operations, and intersperses tool retractions between them.
chain :: Backend a => Double 		-- ^ zSafe : The altitude of tool retractions
	-> [Operation a]	 	-- ^ The list of operations
	-> Operation a			-- ^ The resulting operation
chain zSafe = opsequence . intersperse (retract zSafe) 

-- | Pause operation, takes no arguments
pause :: Backend a => Operation a
pause = bPause

-- | Basic probing operation.
probe :: Backend a => V3 Double 	-- ^ dst : The destination of the probing move
	-> Operation a			-- ^ The resulting operation.
probe dst = do
		tr <- getTransformation
		pbr <- getProbeRate
		move (tr dst) (bProbe pbr)

defCurPos :: Backend a => V3 Double -> Operation a
defCurPos p = do
		tr <- getTransformation
		move (tr p) bDefCurPos

comment :: Backend a => String -> Operation a
comment = bComment

changeTool :: Backend a => Tool -> Operation a
changeTool t = retract 30
		+++ message ("Please place the tool " ++ show t ++ " in the spindle.")
		<* putTool t

-- | Do an operation with a temporary tool.
withTool :: Backend a => Tool 			-- ^ t : The tool to use for the operation
	-> Operation a				-- ^ op : The operation to run with the given tool.
	-> Operation a				-- ^ The resulting operation.
withTool t op = getTool >>= (\mt -> changeTool t +++ op +++ changeTool mt)

-- | Gives a name to an operation. (Used for displaying only an operation and not the others, etc.)
name :: Backend a => String
	-> Operation a
	-> Operation a
name = bName

-- | Displays a message and makes a pause (M00). Compatible with LinuxCNC.
message :: Backend a => String
	-> Operation a
message s = comment ("MSG, " ++ s) +++ pause

-- | Runs an operation with the specified parameters.
runOperation ::	Backend a => (Double, Double, Double, Double) 	-- ^ (fr, pr, pbr, dc) : Feed rate, plunge rate, probe rate, depth of cut (depth of cut must be a negative number)
		-> V3 Double					-- ^ spos : Starting position of the tool	
		-> Tool						-- ^ tool : Default tool 
		-> Operation a					-- ^ op : Operation to run
		-> a
runOperation (fr, pr, pbr, dc) spos tool op = evalState (runReaderT op (id, fr, pr, pbr, dc))  (spos, tool)
	
-- | Runs an operation with default starting transformation, feed rate, plunge rate, probe rate, depth of cut, position and tool.
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
runOperationWithDefaultParams :: Backend a => Operation a 	-- ^ The operation to run
				-> a
runOperationWithDefaultParams = runOperation (100, 30, 10, -0.5) (V3 0 0 0) EndMill{diameter=3, len=42}

