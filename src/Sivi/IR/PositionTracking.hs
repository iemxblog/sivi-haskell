{-|
Module		: Sivi.IR.PositionTracking
Description	: Helper functions to build functions that need to know the position of the tool
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
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

nextPosition :: Instruction -> V3 Double -> V3 Double
nextPosition (Move dst _) _ = dst
nextPosition (Comment _) cp = cp
nextPosition Pause cp = cp
nextPosition (DefCurPos np) _ = np

setPosition :: V3 Double -> TrackedPosition ()
setPosition = put

getPosition :: TrackedPosition (V3 Double)
getPosition = get

trackPosition :: (Instruction -> TrackedPosition a) -> Instruction -> TrackedPosition a
trackPosition f i = do
			r <- f i
			cp <- getPosition
			setPosition (nextPosition i cp)
			return r

mapTrackPosition :: (Instruction -> TrackedPosition a) -> [Instruction] -> [a]
mapTrackPosition f is = evalState (mapM (trackPosition f) is) (V3 0 0 0)
