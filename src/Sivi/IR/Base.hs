{-|
Module          : Sivi.IR.Base
Description     : IR Datatype declaration
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.IR.Base
(
        MoveParams(..)
        , IRInstruction(..)
        , IR(..)
        , getIR
) where

import Linear
import Data.Monoid()
import Control.Monad.RWS
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Operation.Run
import Sivi.Machine
import Sivi.Backend
import Sivi.Approx
import Data.List

-- | Parameters for the 'Move' data constructor.
-- A move is either a Rapid move, or a linear interpolation with a feed rate, or an arc. So there is only one data constructor for moves.
data MoveParams = Rapid 
                | Slow {feedRate :: Double}
                | LinearInterpolation { feedRate :: Double } 
                | Arc { direction :: ArcDirection, center :: V3 Double, feedRate :: Double }
                | Probe {feedRate :: Double }
                deriving (Eq, Show)

-- | Intermediate Representation
data IRInstruction = 
        Move (V3 Double) MoveParams             -- ^ Rapid, Linear interpolation, Arc, ... (all actions that make the tool move)
        | Comment String                        -- ^ Comments
        | Pause                                 -- ^ Pause (waits for user interaction, translated to a M00 GCode). In GRBL, program will stop until Cycle Start is pressed.
        | DefCurPos (V3 Double)         -- ^ Define current position
        deriving (Eq, Show)

newtype IR = IR [IRInstruction] deriving Eq

instance Monoid IR where
        mempty = IR []
        IR xs `mappend` IR ys = IR $ xs ++ ys

instance Backend IR where
        bRapid dst = tell $ IR [Move dst Rapid]
        bSlow fr dst = tell $ IR [Move dst (Slow fr)]
        bFeed fr dst = tell $ IR [Move dst (LinearInterpolation fr)]  
        bArc fr dir center dst = tell $ IR [Move dst (Arc dir center fr)]
        bPause = tell $ IR [Pause]
        bProbe pbr dst = tell $ IR [Move dst (Probe pbr)]
        bDefCurPos dst = tell $ IR [DefCurPos dst]
        bComment s = tell $ IR [Comment s]
        bName _ op = op -- name is ignored

-- | Returns IR code generated from an operation. This is an IR specific version of 'runOperation'.
getIR ::        Machine m =>
                CuttingParameters m          -- Cutting parameters
                -> Operation m IR ()            -- ^ op : Operation to tun
                -> IR                           -- ^ Resulting GCode program
getIR = runOperation

instance Show IR where
    show (IR xs) = (concat . intersperse "\n" . map show) xs

instance Approx IRInstruction where
    (Move v1 mp1) ~= (Move v2 mp2) = (v1 ~= v2) && (mp1 == mp2)
    (DefCurPos v1) ~= (DefCurPos v2) = v1 ~= v2
    iri1 ~= iri2 = iri1 == iri2

instance Approx IR where
    (IR xs1) ~= (IR xs2) = xs1 ~= xs2
