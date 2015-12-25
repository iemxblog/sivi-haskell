{-|
Module          : Sivi.IR.ToGCode
Description     : Conversion of IR to GCode
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.IR.ToGCode
(
        toGCode
) where

import Linear
import Sivi.IR.Base
import Sivi.IR.PositionTracking
import Sivi.GCode 
import Sivi.Operation.Types

-- | Helper function for 'toGCode'
toGCode' :: IRInstruction                       -- ^ The instruction to compile 
        -> TrackedPosition GCodeInstruction     -- ^ The resulting 'GCode' instruction
toGCode' (Move (V3 x y z) Rapid) = return $ G00 (Just x) (Just y) (Just z)
toGCode' (Move (V3 x y z) (LinearInterpolation f)) = return $ G01 (Just x) (Just y) (Just z) (Just f)
toGCode' (Move (V3 x y z) (Arc dir center f)) = do
                                                        cp <- getPosition
                                                        let V3 i j k = center - cp
                                                        let notZero v = if v /= 0 then Just v else Nothing
                                                        return $ g dir (Just x) (Just y) (Just z) (notZero i) (notZero j) (notZero k) (Just f)
                                                where
                                                        g CW = G02
                                                        g CCW = G03
toGCode' (Move (V3 x y z) (Probe f)) = return $ G38d2 (Just x) (Just y) (Just z) (Just f)
toGCode' (Sivi.IR.Base.Comment s) = return $ Sivi.GCode.GComment s
toGCode' Pause = return M00
toGCode' (DefCurPos (V3 x y z)) = return $ G92 (Just x) (Just y) (Just z)

-- | Compiles intermediate representation to 'GCode'
toGCode :: IR           -- ^ The program in intermediate representation
         -> GCode       -- ^ The generated G-Code
toGCode = GCode . mapTrackPosition toGCode'
