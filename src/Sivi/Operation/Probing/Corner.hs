{-|
Module          : Sivi.Operation.Probing.Corner
Description     : Probing operations for corners
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Probing.Corner (
        probeInnerCornerNE
        , probeInnerCornerNW
        , probeInnerCornerSW
        , probeInnerCornerSE
        , probeOuterCornerNE
) where

import Linear hiding (rotate)
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Operation.Probing.Base
import Sivi.Machine
import Sivi.Backend

-- | Probes a corner inside a rectangular pocket, in the North-East direction.
probeInnerCornerNE ::   (Machine m, Backend w) => Double    -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()
probeInnerCornerNE margin probeTool = do
        withTool probeTool $ do
                message "Place the probe 5mm above the corner"
                defCurPos (V3 0 0 5)
                chain 5 [
                        probeZMinus (V3 margin margin 0) margin
                        , probeXPlus (V3 0 (-margin) (-5)) margin
                        , probeYPlus (V3 (-margin) 0 (-5)) margin ]
        message "Tool length measurement"
        probeZMinus (V3 margin margin 0) margin
        message "Finished probing"


-- | Probes a corner inside a rectangular pocket, in the North-West direction.
probeInnerCornerNW ::   (Machine m, Backend w) => Double    -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()
probeInnerCornerNW margin probeTool = rotate 90 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-West direction.
probeInnerCornerSW ::   (Machine m, Backend w) => Double    -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()
probeInnerCornerSW margin probeTool = rotate 180 $ probeInnerCornerNE margin probeTool

-- | Probes a corner inside a rectangular pocket, in the South-East direction.
probeInnerCornerSE ::   (Machine m, Backend w) => Double    -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()
probeInnerCornerSE margin probeTool = rotate 270 $ probeInnerCornerNE margin probeTool

-- | Probes a corner, in the North-East direction.
probeOuterCornerNE ::   (Machine m, Backend w) => Double    -- ^ margin : Probing margin
                        -> Tool                             -- ^ probeTool : Tool used to probe the part
                        -> Operation m w ()
probeOuterCornerNE margin probeTool = do
        withTool probeTool $ do
                message "Place the probe 5mm above the corner"
                defCurPos (V3 0 0 5)
                chain 5 [
                        probeZMinus (V3 margin margin 0) margin
                        , probeXPlus (V3 0 margin (-margin)) margin
                        , probeYPlus (V3 margin 0 (-margin)) margin ]
        message "Tool length measurement"
        probeZMinus (V3 margin margin 0) margin
        message "Finished probing"

