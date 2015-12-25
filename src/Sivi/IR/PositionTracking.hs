{-|
Module          : Sivi.IR.PositionTracking
Description     : Helper functions to build functions that need to know the position of the tool
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.IR.PositionTracking
(
        TrackedPosition
        , getPosition
        , mapTrackPosition
) where

import Control.Monad.State
import Linear
import Sivi.IR.Base

type TrackedPosition a = State (V3 Double) a

-- | Calculates the next position of the tool, when the instruction has been run.
nextPosition :: IRInstruction -> V3 Double -> V3 Double
nextPosition (Move dst _) _ = dst
nextPosition (Comment _) cp = cp
nextPosition Pause cp = cp
nextPosition (DefCurPos np) _ = np

setPosition :: V3 Double -> TrackedPosition ()
setPosition = put

-- | Returns the current position of the tool, which is stored in the 'Control.Monad.State.State' monad.
getPosition :: TrackedPosition (V3 Double)
getPosition = get

-- | Runs a function than needs position tracking, and then updates the position stored in the 'State' monad.
trackPosition :: (IRInstruction -> TrackedPosition a)   -- ^ f : Function to run
                -> IRInstruction                                -- ^ i : Parameter of the function
                -> TrackedPosition a
trackPosition f i = do
                        r <- f i
                        cp <- getPosition
                        setPosition (nextPosition i cp)
                        return r

-- | Maps a function over a list of instructions, with automatic tool position tracking.
-- Position tracking is needed because I J K parameters for arcs are relative to the current position of the tool.
-- When the position is needed in the function, use 'getPosition' to retrieve it.
-- Used in 'Sivi.IR.ArcInterpolation.arcInterpolation' and 'Sivi.IR.ToGCode.toGCode'.
mapTrackPosition :: (IRInstruction -> TrackedPosition a) -> IR -> [a]
mapTrackPosition f (IR is) = evalState (mapM (trackPosition f) is) (V3 0 0 0)
