{-|
Module          : Sivi.Operation.Probing.Cylinder
Description     : Probing operations for cylinders
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Probing.Cylinder (
        probeHorizontalCylinderRight
        , probeOuterCylinder
) where

import Linear
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Operation.Probing.Base
import Sivi.Machine
import Sivi.Backend

-- | Probes a laying cylinder (placed horizontally in a vise). Probes on the right side for the X axis.
probeHorizontalCylinderRight :: (Machine m, Backend w) => Double    -- ^ d : Diameter of the cylinder
                        -> Double                                   -- ^ l : Length of the cylinder
                        -> Double                                   -- ^ margin : Probing margin
                        -> Tool                                     -- ^ probetool : Tool used to probe the part
                        -> Operation m w ()                         -- ^ Resulting operation
probeHorizontalCylinderRight d l margin probetool = do
        withTool probetool $ do
            message "Place the probe 5mm above the right side of the strut, centered on the axis of the cylinder"
            defCurPos (V3 l (d/2) 5)
            chain 5 [
                probeZMinus (V3 l (d/2) 0) margin
                , probeXMinus (V3 l (d/2) (-d/2)) margin
                , probeYPlus (V3 0 0 (-3*d/4)) margin ]
        message "Don't forget to put the probe connectors for tool length measurement."
        probeZMinus (V3 0 (d/2) 0) margin
        message "Remove the probe connectors"

-- | Probes a vertical cylinder. Touches the outside surface.
probeOuterCylinder ::   (Machine m, Backend w) => Double    -- ^ d : Diameter of the cylinder
                        -> Double                           -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()                 -- ^ Resulting operation
probeOuterCylinder d margin probeTool = do
        withTool probeTool $ do
                message "Place the probe 5mm above the center of the cylinder"
                defCurPos (V3 0 0 5)
                chain 5 [
                        probeZMinus (V3 (d/4) 0 0) margin
                        , probeXPlus (V3 (-d/2) 0 (-5)) margin
                        , probeYPlus (V3 0 (-d/2) (-5)) margin ]
        message "Tool length measurement"
        probeZMinus (V3 (d/4) 0 0) margin
        message "Finished probing."
