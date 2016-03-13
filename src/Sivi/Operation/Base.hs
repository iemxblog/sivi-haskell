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
        , slow
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
        , (+^+)
        , chain
        , pause
        , probe
        , defCurPos
        , comment
        , withTool
        , name
        , message
        , zigzag
)
where

import Sivi.Machine
import Sivi.Backend
import Sivi.Operation.Types
import Linear hiding (rotate)
import qualified Linear (rotate)
import Control.Monad.RWS
import Control.Applicative()
import Data.List
import Data.Monoid()

-- | Returns the current transformation
getTransformation :: Monoid w => Operation m w Transformation
getTransformation = liftM eTransformation ask

-- | Returns the origin of an operation
getOrigin :: Monoid w => Operation m w (V3 Double)
getOrigin = do 
                tr <- getTransformation
                return $ tr (V3 0 0 0)

-- | Returns the current feed rate
getFeedRate :: Monoid w => Operation m w Double
getFeedRate = liftM eFeedRate ask

-- | Returns the current plunge rate
getPlungeRate :: Monoid w => Operation m w Double
getPlungeRate = liftM ePlungeRate ask

-- | Returns the current probe rate
getProbeRate :: Monoid w => Operation m w Double
getProbeRate = liftM eProbeRate ask

-- | Returns the current depth of cut
getDepthOfCut :: Monoid w => Operation m w Double
getDepthOfCut = liftM eDepthOfCut ask

-- | Calls an operation with the specified transformation
withTransformation :: Monoid w =>
                        Transformation          -- ^ ntr : The new transformation
                        -> Operation m w a      -- ^ The operation to call with the specified transformation
                        -> Operation m w a      -- ^ The resulting operation
withTransformation ntr = local (\e -> let tr = eTransformation e in e {eTransformation = tr . ntr})

-- | Calls an operation with the specified feed rate
withFeedRate :: Monoid w =>
                Double                      -- ^ nfr : The new feed rate
                -> Operation m w a          -- ^ The operation to call with the new feed rate
                -> Operation m w a          -- ^ The resulting operation
withFeedRate nfr = local (\e -> e {eFeedRate = nfr})

-- | Calls an operation with the specified plunge rate
withPlungeRate :: Monoid w =>
                Double                      -- ^ npr : The new plunge rate
                -> Operation m w a          -- ^ The operation to call with the new plunge rate
                -> Operation m w a          -- ^ The resulting operation
withPlungeRate npr = local (\e -> e {ePlungeRate = npr})

-- | Calls an operation with the specified probe rate
withProbeRate :: Monoid w =>
                Double                  -- ^ npbr : The new probe rate
                -> Operation m w a      -- ^ The operation to call with the new probe rate
                -> Operation m w a      -- ^ The resulting operation
withProbeRate npbr = local (\e -> e {eProbeRate = npbr})

-- | Calls an operation with the specified depth of cut. (depth of cut must be a negative number)
withDepthOfCut :: Monoid w =>
                Double                  -- ^ ndc : The new depth of cut
                -> Operation m w a      -- ^ The operation to call with the new depth of cut
                -> Operation m w a      -- ^ The resulting operation
withDepthOfCut ndc = local (\e -> e {eDepthOfCut = ndc})
        
-- | Returns the machine's current position (from the State monad)
getCurrentPosition :: Monoid w => Operation m w (V3 Double)
getCurrentPosition = liftM sCurrentPosition get

-- | Sets the current position
putCurrentPosition :: Monoid w => V3 Double -> Operation m w ()
putCurrentPosition cp = do
                                s <- get
                                put s {sCurrentPosition = cp}

-- | Returns the current tool
getTool :: Monoid w => Operation m w Tool
getTool = liftM sTool get

-- | Sets the current tool
putTool :: Monoid w => Tool -> Operation m w ()
putTool t = do
                s <- get
                put s {sTool = t}

-- | Returns the current tool's diameter
getToolDiameter :: Monoid w => Operation m w Double
getToolDiameter = do
                        tool <- getTool
                        return $ diameter tool

-- | Do-nothing operation
noOp :: Monoid w => Operation m w ()
noOp = return ()

-- Variables :
-- dst : destination
-- tr : transformation
-- fr : feed rate
-- pr : plunge rate
-- dc : depth of cut

-- | Helper function
move :: Backend w => V3 Double -> (V3 Double -> Operation m w ()) -> Operation m w ()
move dst op = op dst <* putCurrentPosition dst

-- | Checks if we are already at destination. If so, we don't do anything.
checkDst :: Monoid w => V3 Double -> Operation m w () -> Operation m w ()
checkDst dst op = do
                    cp <- getCurrentPosition
                    tr <- getTransformation
                    if cp == tr dst then noOp else op

-- | Checks if we are already at destination (NT : does not apply transformation during checking). If so, we don't do anything.
checkDstNT :: Monoid w => V3 Double -> Operation m w () -> Operation m w ()
checkDstNT dst op = do
                    cp <- getCurrentPosition
                    if cp == dst then noOp else op

-- | Rapid positioning
rapid :: Backend w => V3 Double         -- ^ dst : Destination
         -> Operation m w ()            -- Resulting operation
rapid dst = checkDst dst $ do
                tr <- getTransformation
                move (tr dst) bRapid

-- | Rapid positioning, but does not apply the current transformation. Not meant to be exported, only used as an internal helper function. (NT means "no transformation")
rapidNT :: Backend w => V3 Double       -- ^ dst : Destination
        -> Operation m w ()             -- ^ Resulting operation
rapidNT dst = checkDstNT dst $ move dst bRapid

-- | Linear interpolation, but only when the tool does not cut. It is for slow moves. See 'bSlow' for more information.
slow :: Backend w => V3 Double          -- ^ dst : Destination
         -> Operation m w ()            -- Resulting operation
slow dst = checkDst dst $ do
                tr <- getTransformation
                fr <- getFeedRate       
                move (tr dst) (bSlow fr)

-- | Linear interpolation, but only when the tool does not cut. It is for slow moves. See 'bSlow' for more information.
-- Version with no application of transformation (NT means no transformation). Used in 'approach' only.
slowNT :: Backend w => V3 Double        -- ^ dst : Destination
         -> Operation m w ()            -- Resulting operation
slowNT dst = checkDst dst $ do
                fr <- getFeedRate       
                move dst (bSlow fr)


-- | Linear interpolation (with the default feedrate), when the tools cuts matter.
feed :: Backend w => V3 Double          -- ^ dst : Destination
         -> Operation m w ()            -- Resulting operation
feed dst = checkDst dst $ do
                tr <- getTransformation
                fr <- getFeedRate       
                move (tr dst) (bFeed fr)

-- | Circular interpolation
arc :: Backend w => ArcDirection        -- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
        -> V3 Double                    -- ^ center : The center of the arc (relative to the origin of the operation)
        -> V3 Double                    -- ^ dst : The destination point
        -> Operation m w ()             -- ^ Resulting operation
arc dir center dst = do
                        tr <- getTransformation
                        fr <- getFeedRate
                        move (tr dst) (bArc fr dir (tr center))

-- | Circular interpolation, but does not apply the current transformation. Exported only for 'circleFromHere'. Only used as an internal helper function. (NT means "no transformation")
arcNT :: Backend w => ArcDirection      -- ^ dir : CW (Clockwise) or CCW (Counter-clockwise)
        -> V3 Double                    -- ^ center : The center of the arc (relative to the origin of the operation)
        -> V3 Double                    -- ^ dst : The destination point
        -> Operation m w ()             -- ^ Resulting operation
arcNT dir center dst = do
                        fr <- getFeedRate
                        move dst (bArc fr dir center)

-- | Linear interpolation (with the plunge feedrate)
plunge :: Backend w => V3 Double        -- ^ dst : Destination
         -> Operation m w ()            -- ^ Resulting operation
plunge dst = checkDst dst $ do
                tr <- getTransformation
                pr <- getPlungeRate
                move (tr dst) (bFeed pr)

-- | Tool retraction
retract :: Backend w => Double          -- ^ z_safe : destination on the Z axis
        -> Operation m w ()             -- ^ Resulting operation
retract z_safe = do 
                        V3 _ _ zo <- getOrigin
                        V3 x y _ <- getCurrentPosition
                        rapidNT (V3 x y (zo+z_safe))

-- | Rapid in the XY plane (helper function for 'approach')
rapid_xy :: Backend w => V3 Double      -- ^ dst : Destination
         -> Operation m w ()            -- ^ Resulting operation
rapid_xy dst = do
                        tr <- getTransformation
                        V3 _ _ z <- getCurrentPosition  
                        let V3 xd yd _ = tr dst 
                        rapidNT (V3 xd yd z)

-- | Rapid in the xy plane + rapid plunge with margin (2 * depth of cut above destination) + plunge (with plunge rate) to destination
approach :: Backend w => V3 Double      -- ^ dst : Destination
         -> Operation m w ()            -- ^ Resulting operation
approach dst = do
        tr <- getTransformation
        let V3 xd yd zd = tr dst
        V3 x y z <- getCurrentPosition  
        dc <- getDepthOfCut
        let zmin = zd + 2 * abs dc
        if z < zmin then retract zmin else noOp
        if (x /= xd || y/= yd) then rapid_xy dst else noOp
        if z > zmin then rapidNT (tr dst + V3 0 0 (2 * abs dc)) else noOp
        slowNT (tr dst + V3 0 0 (abs dc))
        plunge dst

-- | Same as approach, but plunge with rapid move only
approach_rapid :: Backend w => V3 Double        -- ^ dst : Destination
                -> Operation m w ()             -- ^ Resulting operation
approach_rapid dst = rapid_xy dst >> rapid dst

-- | Translate an operation
translate :: Monoid w =>
        V3 Double               -- ^ v : Translation vector
        -> Operation m w a      -- ^ o : Operation to translate
        -> Operation m w a      -- Resulting operation
translate v = withTransformation (+v)

-- | Rotates an operation in the XY plane.
rotate :: Monoid w =>
        Double                  -- ^ a : angle (in degrees)
        -> Operation m w a      -- ^ Operation to rotate
        -> Operation m w a      -- ^ Resulting operation
rotate a = withTransformation (Linear.rotate (axisAngle (V3 0 0 1) ar))
        where ar = a * pi / 180

-- | Symmetry about the X axis.
symmetryX ::    Monoid w =>
                Operation m w a
                -> Operation m w a 
symmetryX = withTransformation (\(V3 x y z) -> V3 x (-y) z)

-- | Symmetry about the Y axis.
symmetryY ::    Monoid w =>
                Operation m w a 
                -> Operation m w a 
symmetryY = withTransformation (\(V3 x y z) -> V3 (-x) y z)

-- | Chain two operations, with tool retraction at z=1 between the operations.
(+^+) :: (Backend w) => Operation m w ()    -- ^ o1 : Operation 1
        -> Operation m w ()                 -- ^ o2 : Operation 2
        -> Operation m w ()                 -- ^ Operation 1, retract at z=1, operation 2
o1 +^+ o2 = o1 >> retract 1 >> o2

-- | Chains a list of operations, and intersperses tool retractions between them.
chain :: Backend w => Double            -- ^ zSafe : The altitude of tool retractions
        -> [Operation m w ()]           -- ^ The list of operations
        -> Operation m w ()             -- ^ The resulting operation
chain zSafe = sequence_ . intersperse (retract zSafe) 

-- | Pause operation, takes no arguments
pause :: Backend w => Operation m w () 
pause = bPause

-- | Basic probing operation.
probe :: Backend w => V3 Double         -- ^ dst : The destination of the probing move
        -> Operation m w ()             -- ^ The resulting operation.
probe dst = checkDst dst $ do
                tr <- getTransformation
                pbr <- getProbeRate
                move (tr dst) (bProbe pbr)

defCurPos :: Backend w => V3 Double -> Operation m w ()
defCurPos p = checkDst p $ do
                tr <- getTransformation
                move (tr p) bDefCurPos

comment :: Backend w => String -> Operation m w () 
comment = bComment

-- | Do an operation with a temporary tool.
withTool :: (Machine m, Backend w) => Tool      -- ^ t : The tool to use for the operation
        -> Operation m w a                     -- ^ op : The operation to run with the given tool.
        -> Operation m w a                     -- ^ The resulting operation.
withTool t op = do
                    mt <- getTool 
                    changeTool t 
                    rv <- op 
                    changeTool mt
                    return rv

-- | Gives a name to an operation. (Used for displaying only an operation and not the others, etc.)
name :: Backend w => String
        -> Operation m w () 
        -> Operation m w () 
name = bName

-- | Displays a message and makes a pause (M00). Compatible with LinuxCNC.
message :: Backend w => String
        -> Operation m w () 
message s = comment ("MSG, " ++ s) >> pause

-- | See 'zigzag'
zigzag' :: Backend w => Bool -> [[V3 Double]] -> Operation m w ()
zigzag' _ [] = noOp
zigzag' b (x:xs) = sequence_ [feed p | p <- ps] >> zigzag' (not b) xs
        where
                ps = case b of
                        True -> reverse x
                        False -> x

-- | Function used to build zigzag operations. 
zigzag :: Backend w => 
        [[V3 Double]]           -- ^ List of paths, every other path is reversed to make zig-zags.
        -> Operation m w () 
zigzag xs 
        | null (concat xs) = noOp
        | otherwise = approach firstPoint >> zigzag' False xs
                where firstPoint = head . concat $ xs
        
