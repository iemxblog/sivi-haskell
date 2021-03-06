{-|
Module          : Sivi.Backend
Description     : Backend class definition
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Backend
(
        Backend(..)
) where

import Sivi.Operation.Types
import Data.Monoid()
import Linear

class Monoid w => Backend w where
        -- | Rapid move (G00)
        bRapid :: V3 Double     -- ^ dst : Destination of the rapid move
                -> Operation m w ()
        -- | Slow move, used in 'approach'. G01, but only for approach moves, not machining moves.
        -- We need to separate bSlow and bFeed in order to calculate BoundingBox : we need to know when
        -- machining is done. bFeed -> machining is done, bSlow -> slow, but no machining.
        bSlow :: Double         -- ^ fr : Feed rate
                -> V3 Double    -- ^ dst : Destination
                -> Operation m w ()
        -- | Linear interpolation (G01). For machining moves.
        bFeed :: Double         -- ^ fr : Feed rate
                -> V3 Double    -- ^ dst : Destination
                -> Operation m w ()
        -- | Arc (G02 or G03)
        bArc :: Double          -- ^ feed rate
                -> ArcDirection -- ^ dir : Direction of the arc : clockwise or counterclockwise
                -> V3 Double    -- ^ center : Center of the arc
                -> V3 Double    -- ^ dst : Destination
                -> Operation m w ()
        -- | Pause (M00)
        bPause :: Operation m w ()
        -- | Probe (G38.2)
        bProbe :: Double        -- ^ Probe rate
                -> V3 Double    -- ^ dst
                -> Operation m w ()
        -- | Defines current position (G92)
        bDefCurPos :: V3 Double         -- ^ Position
                        -> Operation m w ()
        -- | Comment
        bComment :: String -> Operation m w ()
        -- | Gives a name to an operation
        bName :: String -> Operation m w () -> Operation m w ()
        bName n op = bComment ("Beginning of operation \""++ n ++ "\"") >> op >> bComment ("End of operation \"" ++ n ++"\"")
