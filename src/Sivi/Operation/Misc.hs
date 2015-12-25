{-|
Module          : Sivi.Operation.Misc
Description     : Miscellaneous operations (saw, drill, ...)
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}

module Sivi.Operation.Misc (
        saw_left
        , drill
        , ArcCompensation(..)
        , compensatedArc
)
where
import Linear
import Data.Monoid
import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Operation.Repetition
import Sivi.Backend
import Sivi.Misc.ArcInterpolation

-- | Generates a single pass of a sawing operation. Cuts in the Y direction. Tool radius compensation is done on the left. P means "Pass".
saw_leftP :: Backend a => Double        -- ^ w : Width of the cut (tool radius compensation is automatic)
        -> Operation a                  -- ^ Resulting operation
saw_leftP w = do
                df <- getToolDiameter
                op1 <- approach $ V3 (-df/2) (-df/2) 0 
                op2 <- feed $ V3 (-df/2) (w+df/2) 0
                return $ mconcat [op1, op2]

-- | Sawing operation. Cuts in the Y direction. Tool radius compensation is done on the left. 
saw_left :: Backend a => Double         -- ^ w : Width of the cut (tool radius compensation is automatic)
        -> Double                       -- ^ depth : Depth of the cut
        -> Double                       -- ^ retraction : Altitude to go to between each pass
        -> Operation a                  -- ^ Resulting operation
saw_left w depth retraction = zRepetition depth (Just retraction) (const $ saw_leftP w)

-- | Generates a single pass of a drilling operation. P means "pass".
drillP :: Backend a => Operation a
drillP = approach (V3 0 0 0)

-- | Drilling operation. Modify the depth of cut with 'withDepthOfCut' if you want to drill faster.
drill :: Backend a => Double    -- ^ depth : Depth of the hole
        -> Double               -- ^ retraction : Altitude to go to in order to evacuate the chips
        -> Operation a          -- ^ Resulting operation
drill depth retraction = zRepetition depth (Just retraction) (const drillP)

data ArcCompensation = InnerCompensation | OuterCompensation deriving (Eq, Show)

-- | Interpolated arc with tool radius compensation. Approach move is included in it.
compensatedArc :: Backend a =>
                ArcCompensation         -- ^ comp : Inner or outer compensation of the arc
                -> V3 Double            -- ^ from : Starting point
                -> V3 Double            -- ^ to : End point
                -> V3 Double            -- ^ cen : Center of the arc
                -> ArcDirection         -- ^ dir : Direction ('CW' : clockwise, or 'CCW' : counterclockwise)
                -> Double               -- ^ ai : Angle increment (in degrees)
                -> Operation a
compensatedArc comp from to cen dir ai = do
        td <- getToolDiameter
        let from' = case comp of
                InnerCompensation -> compensate cen from (-td/2)
                OuterCompensation -> compensate cen from (td/2)
        let to' = case comp of
                InnerCompensation -> compensate cen to (-td/2)
                OuterCompensation -> compensate cen to (td/2)
        approach from' +++ (opsequence . map feed) (arcInterpolation from' to' cen dir ai)
        where compensate o p c = o +  ((norm (p-o)) + c) *^ (signorm (p-o))
