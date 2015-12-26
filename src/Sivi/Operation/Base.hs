{-|
Module          : Sivi.Operation.Base
Description     : Base operations
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
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
        , chain
        , runOperation
        , defaultCuttingParameters
        , pause
        , probe
        , defCurPos
        , comment
        , changeTool
        , withTool
        , name
        , message
        , zigzag
)
where

import Sivi.Backend
import Sivi.Operation.Types
import Linear hiding (rotate)
import qualified Linear (rotate)
import Control.Monad.RWS
import Control.Applicative()
import Data.List
import Data.Monoid()

-- | Returns the current transformation
getTransformation :: Monoid w => Operation' w Transformation
getTransformation = do
                        (tr, _, _, _, _) <- ask
                        return tr

-- | Returns the origin of an operation
getOrigin :: Monoid w => Operation' w (V3 Double)
getOrigin = do 
                tr <- getTransformation
                return $ tr (V3 0 0 0)

-- | Returns the current feed rate
getFeedRate :: Monoid w => Operation' w Double
getFeedRate = do
                (_, fr, _, _, _) <- ask
                return fr

-- | Returns the current plunge rate
getPlungeRate :: Monoid w => Operation' w Double
getPlungeRate = do
                (_, _, pr, _, _) <- ask
                return pr

-- | Returns the current probe rate
getProbeRate :: Monoid w => Operation' w Double
getProbeRate = do
                (_, _, _, pbr, _) <- ask
                return pbr


-- | Returns the current depth of cut
getDepthOfCut :: Monoid w => Operation' w Double
getDepthOfCut = do
                (_, _, _, _, dc) <- ask
                return dc

-- | Calls an operation with the specified transformation
withTransformation :: Monoid a =>
                        Transformation          -- ^ ntr : The new transformation
                        -> Operation a          -- ^ The operation to call with the specified transformation
                        -> Operation a          -- ^ The resulting operation
withTransformation ntr = local (\(tr, fr, pr, pbr, dc) -> (tr . ntr, fr, pr, pbr, dc))

-- | Calls an operation with the specified feed rate
withFeedRate :: Monoid a =>
                Double                  -- ^ nfr : The new feed rate
                -> Operation a          -- ^ The operation to call with the new feed rate
                -> Operation a          -- ^ The resulting operation
withFeedRate nfr = local (\(tr, _, pr, pbr, dc) -> (tr, nfr, pr, pbr, dc))

-- | Calls an operation with the specified plunge rate
withPlungeRate :: Monoid a =>
                Double                -- ^ npr : The new plunge rate
                -> Operation a          -- ^ The operation to call with the new plunge rate
                -> Operation a          -- ^ The resulting operation
withPlungeRate npr = local (\(tr, fr, _, pbr, dc) -> (tr, fr, npr, pbr, dc))

-- | Calls an operation with the specified probe rate
withProbeRate :: Monoid a =>
                Double                 -- ^ npbr : The new probe rate
                -> Operation a          -- ^ The operation to call with the new probe rate
                -> Operation a          -- ^ The resulting operation
withProbeRate npbr = local (\(tr, fr, pr, _, dc) -> (tr, fr, pr, npbr, dc))

-- | Calls an operation with the specified depth of cut
withDepthOfCut :: Monoid a =>
                Double                -- ^ ndc : The new depth of cut
                -> Operation a          -- ^ The operation to call with the new depth of cut
                -> Operation a          -- ^ The resulting operation
withDepthOfCut ndc = local (\(tr, fr, pr, pbr, _) -> (tr, fr, pr, pbr, ndc))
        
-- | Returns the machine's current position (from the State monad)
getCurrentPosition :: Monoid w => Operation' w (V3 Double)
getCurrentPosition = liftM fst get

-- | Sets the current position
putCurrentPosition :: Monoid w => V3 Double -> Operation' w ()
putCurrentPosition cp = do
                                (_, t) <- get
                                put (cp, t)

-- | Returns the current tool
getTool :: Monoid w => Operation' w Tool
getTool = liftM snd get

-- | Sets the current tool
putTool :: Monoid w => Tool -> Operation' w ()
putTool t = do
                (cp, _) <- get
                put (cp, t)

-- | Returns the current tool's diameter
getToolDiameter :: Monoid w => Operation' w Double
getToolDiameter = do
                        tool <- getTool
                        return $ diameter tool

-- | Do-nothing operation
noOp :: Monoid m => Operation m
noOp = return ()

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
rapid :: Backend a => V3 Double         -- ^ dst : Destination
         -> Operation a                 -- Resulting operation
rapid dst = do
                tr <- getTransformation
                move (tr dst) bRapid

-- | Rapid positioning, but does not apply the current transformation. Not meant to be exported, only used as an internal helper function. (NT means "no transformation")
rapidNT :: Backend a => V3 Double       -- ^ dst : Destination
        -> Operation a                  -- ^ Resulting operation
rapidNT dst = move dst bRapid

-- | Linear interpolation (with the default feedrate)
feed :: Backend a => V3 Double          -- ^ dst : Destination
         -> Operation a                 -- Resulting operation
feed dst = do
                tr <- getTransformation
                fr <- getFeedRate       
                move (tr dst) (bFeed fr)

-- | Circular interpolation
arc :: Backend a => ArcDirection        -- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
        -> V3 Double                    -- ^ center : The center of the arc (relative to the origin of the operation)
        -> V3 Double                    -- ^ dst : The destination point
        -> Operation a                  -- ^ Resulting operation
arc dir center dst = do
                        tr <- getTransformation
                        fr <- getFeedRate
                        move (tr dst) (bArc fr dir center)

-- | Circular interpolation, but does not apply the current transformation. Exported only for 'circleFromHere'. Only used as an internal helper function. (NT means "no transformation")
arcNT :: Backend a => ArcDirection      -- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
        -> V3 Double                    -- ^ center : The center of the arc (relative to the origin of the operation)
        -> V3 Double                    -- ^ dst : The destination point
        -> Operation a                  -- ^ Resulting operation
arcNT dir center dst = do
                        fr <- getFeedRate
                        move dst (bArc fr dir center)

-- | Linear interpolation (with the plunge feedrate)
plunge :: Backend a => V3 Double        -- ^ dst : Destination
         -> Operation a                 -- ^ Resulting operation
plunge dst = do
                tr <- getTransformation
                pr <- getPlungeRate
                move (tr dst) (bFeed pr)

-- | Tool retraction
retract :: Backend a => Double          -- ^ z_safe : destination on the Z axis
        -> Operation a                  -- ^ Resulting operation
retract z_safe = do 
                        V3 _ _ zo <- getOrigin
                        V3 x y _ <- getCurrentPosition
                        rapidNT (V3 x y (zo+z_safe))

-- | Rapid in the XY plane (helper function for 'approach')
rapid_xy :: Backend a => V3 Double      -- ^ dst : Destination
         -> Operation a                 -- ^ Resulting operation
rapid_xy dst = do
                        tr <- getTransformation
                        V3 _ _ z <- getCurrentPosition  
                        let V3 xd yd _ = tr dst 
                        rapidNT (V3 xd yd z)

-- | Rapid in the xy plane + rapid plunge with margin (2 * depth of cut above destination) + plunge (with plunge rate) to destination
approach :: Backend a => V3 Double      -- ^ dst : Destination
         -> Operation a                 -- ^ Resulting operation
approach dst = do
        tr <- getTransformation
        let V3 _ _ zd = tr dst
        V3 _ _ z <- getCurrentPosition  
        dc <- getDepthOfCut
        rapid_xy dst 
        if (z-zd) > 2 * abs dc then rapidNT (tr dst + V3 0 0 (2 * abs dc)) else noOp
        plunge dst

-- | Same as approach, but plunge with rapid move only
approach_rapid :: Backend a => V3 Double        -- ^ dst : Destination
                -> Operation a                  -- ^ Resulting operation
approach_rapid dst = rapid_xy dst >> rapid dst

-- | Translate an operation
translate :: Monoid a =>
        V3 Double          -- ^ v : Translation vector
        -> Operation a          -- ^ o : Operation to translate
        -> Operation a          -- Resulting operation
translate v = withTransformation (+v)

-- | Rotates an operation in the XY plane.
rotate :: Monoid a =>
        Double                -- ^ a : angle (in degrees)
        -> Operation a          -- ^ Operation to rotate
        -> Operation a          -- ^ Resulting operation
rotate a = withTransformation (Linear.rotate (axisAngle (V3 0 0 1) ar))
        where ar = a * pi / 180

-- | Symmetry about the X axis.
symmetryX ::    Monoid a =>
                Operation a
                -> Operation a
symmetryX = withTransformation (\(V3 x y z) -> V3 x (-y) z)

-- | Symmetry about the Y axis.
symmetryY ::    Monoid a =>
                Operation a
                -> Operation a
symmetryY = withTransformation (\(V3 x y z) -> V3 (-x) y z)

-- | Chains a list of operations, and intersperses tool retractions between them.
chain :: Backend a => Double            -- ^ zSafe : The altitude of tool retractions
        -> [Operation a]                -- ^ The list of operations
        -> Operation a                  -- ^ The resulting operation
chain zSafe = sequence_ . intersperse (retract zSafe) 

-- | Pause operation, takes no arguments
pause :: Backend a => Operation a
pause = bPause

-- | Basic probing operation.
probe :: Backend a => V3 Double         -- ^ dst : The destination of the probing move
        -> Operation a                  -- ^ The resulting operation.
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
changeTool t = retract 20
                >> message ("Please place the tool " ++ show t ++ " in the spindle.")
                <* putTool t

-- | Do an operation with a temporary tool.
withTool :: Backend a => Tool                   -- ^ t : The tool to use for the operation
        -> Operation a                          -- ^ op : The operation to run with the given tool.
        -> Operation a                          -- ^ The resulting operation.
withTool t op = getTool >>= (\mt -> changeTool t >> op >> changeTool mt)

-- | Gives a name to an operation. (Used for displaying only an operation and not the others, etc.)
name :: Backend a => String
        -> Operation a
        -> Operation a
name = bName

-- | Displays a message and makes a pause (M00). Compatible with LinuxCNC.
message :: Backend a => String
        -> Operation a
message s = comment ("MSG, " ++ s) >> pause

-- | Runs an operation with the specified parameters.
runOperation :: Backend a => 
                CuttingParameters
                -> Operation a                                  -- ^ op : Operation to run
                -> a
runOperation (CuttingParameters tr fr pr pbr dc ipos itool) op = w
    where (_, _, w) = runRWS op (tr, fr, pr, pbr, dc) (ipos, itool)
        
-- | Default cutting parameters.
defaultCuttingParameters :: CuttingParameters
defaultCuttingParameters = CuttingParameters {transformation = id, feedRate = 100, plungeRate = 30, probeRate = 10, depthOfCut = -0.5, initialPosition = V3 0 0 0, initialTool = EndMill{diameter = 3, len=42}}


-- | See 'zigzag'
zigzag' :: Backend a => Bool -> [[V3 Double]] -> Operation a
zigzag' _ [] = noOp
zigzag' b (x:xs) = sequence_ [feed p | p <- ps] >> zigzag' (not b) xs
        where
                ps = case b of
                        True -> reverse x
                        False -> x

-- | Function used to build zigzag operations. 
zigzag :: Backend a => 
        [[V3 Double]]   -- ^ List of paths, every other path is reversed to make zig-zags.
        -> Operation a
zigzag xs 
        | null (concat xs) = noOp
        | otherwise = approach firstPoint >> zigzag' False xs
                where firstPoint = head . concat $ xs
        
