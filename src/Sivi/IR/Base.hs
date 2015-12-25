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
        , Tree(..)
        , IRTree
        , flatten
) where

import Linear
import Data.Monoid()
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Backend

-- | Parameters for the 'Move' data constructor.
-- A move is either a Rapid move, or a linear interpolation with a feed rate, or an arc. So there is only one data constructor for moves.
data MoveParams = Rapid 
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

newtype IR = IR [IRInstruction] deriving (Eq, Show)

instance Monoid IR where
        mempty = IR []
        IR xs `mappend` IR ys = IR $ xs ++ ys

instance Backend IR where
        bRapid dst = return $ IR [Move dst Rapid]
        bFeed fr dst = return $ IR [Move dst (LinearInterpolation fr)]  
        bArc fr dir center dst = return $ IR [Move dst (Arc dir center fr)]
        bPause = return $ IR [Pause]
        bProbe pbr dst = return $ IR [Move dst (Probe pbr)]
        bDefCurPos dst = return $ IR [DefCurPos dst]
        bComment s = return $ IR [Comment s]
        bName _ op = op -- name is ignored

-- | Returns IR code generated from an operation. This is an IR specific version of 'runOperation'.
getIR ::        CuttingParameters
                -> Operation IR                 -- ^ op : Operation to tun
                -> IR                           -- ^ Resulting GCode program
getIR = runOperation

data Tree v a = 
        Leaf a
        | Node v [Tree v a]

-- | Tree of instructions
type IRTree = Tree String IRInstruction

-- | Transforms a tree of intructions to a list of instructions. Annotations are transformed into comments.
-- It could be possible to make a more general function which accepts more types, but would probably be overkill ??
flatten :: IRTree-> IR
flatten (Leaf i) = IR [i]
flatten (Node v ts) = opening `mappend` (mconcat . map flatten) ts `mappend` closing
        where
                opening = if v == "" then mempty else IR [Comment ("> " ++ v)] 
                closing = if v == "" then mempty else IR [Comment ("< " ++ v)]
