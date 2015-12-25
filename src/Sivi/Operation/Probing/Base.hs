{-|
Module          : Sivi.Operation.Probing.Base
Description     : Probing operations
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Probing.Base (
        probeXMinus
        , probeXPlus
        , probeYMinus
        , probeYPlus
        , probeZMinus
) where

import Linear
import Data.Monoid()
import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Backend

-- | To be used with translate to define the starting point of the probe move.
probeHelper ::  Backend a => V3 Double  -- ^ dir : Direction
                -> Bool                 -- ^ compFlag : True -> tool radius compensation, False -> no tool radius compensation
                -> V3 Double            -- ^ dst : Destination
                -> Double               -- ^ margin : Distance between initial probe position and destination
                -> Operation a          -- ^ Resulting Operation
probeHelper dir compFlag dst margin = 
        translate dst $ do
                td <- getToolDiameter
                let comp = case compFlag of
                        True -> td/2
                        False -> 0
                let initPos = (-1)*(margin+comp) *^ dir
                o1 <- approach_rapid initPos
                o2 <- probe ((margin+comp) *^ dir)
                o3 <- defCurPos (((-1) * comp) *^ dir)
                o4 <- rapid initPos
                return $ mconcat [o1, o2, o3, o4]
                        
        
-- | Probes a part in the X direction, descending tool coordinate.
probeXMinus ::  Backend a => V3 Double          -- ^ Point to probe
                 -> Double                      -- ^ Margin (distance between point to probe and initial tool position)
                 -> Operation a
probeXMinus = probeHelper (V3 (-1) 0 0) True

-- | Probes a part in the X direction, ascending tool coordinate.
probeXPlus ::   Backend a => V3 Double          -- ^ Point to probe
                -> Double                       -- ^ Margin (distance between point to probe and initial tool position)
                -> Operation a
probeXPlus = probeHelper (V3 1 0 0) True

-- | Probes a part in the Y direction, descending tool coordinate.
probeYMinus ::  Backend a => V3 Double          -- ^ Point to probe
                -> Double                       -- ^ Margin (distance between point to probe and initial tool position)
                -> Operation a
probeYMinus = probeHelper (V3 0 (-1) 0) True

-- | Probes a part in the Y direction, ascending tool coordinate.
probeYPlus ::   Backend a => V3 Double          -- ^ Point to probe
                -> Double                       -- ^ Margin (distance between point to probe and initial tool position)
                -> Operation a
probeYPlus = probeHelper (V3 0 1 0) True

-- | Probes a part in the Z direction, descending tool coordinate.
probeZMinus ::  Backend a => V3 Double          -- ^ Point to probe
                -> Double                       -- ^ Margin (distance between point to probe and initial tool position) 
                -> Operation a
probeZMinus dst margin = do
        t <- getTool
        case t of
                EndMill {} -> probeHelper (V3 0 0 (-1)) False dst margin
                BallEndMill {} -> probeHelper (V3 0 0 (-1)) True dst margin
                ProbeTool {} -> probeHelper (V3 0 0 (-1)) False dst margin

