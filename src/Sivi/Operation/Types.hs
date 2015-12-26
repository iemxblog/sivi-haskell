{-|
Module          : Sivi.Operation.Types
Description     : Type declarations
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Types
(
        Transformation
        , Operation
        , Operation'
        , Tool(..)
        , ArcDirection(..)
        , CuttingParameters(..)
) where

import Linear
import Control.Monad.RWS

type Transformation = V3 Double -> V3 Double

-- | The Operation type.
-- Parameters are :
--
-- * Transformation : the current 'Transformation'
--
-- * Double : Feed rate
--
-- * Double : Plunge rate
--
-- * Double : Probe rate
--
-- * Double : Depth of cut
--
-- * Double (in the State part of the Monad) : The current machine position
--
-- * Tool (in the State part of the Monad) : The current tool
type Operation a = RWS (Transformation, Double, Double, Double, Double) a (V3 Double, Tool) ()

-- | Auxiliary operation type, for functions that return something like 'getTransformation', etc.
type Operation' w a = RWS (Transformation, Double, Double, Double, Double) w (V3 Double, Tool) a


-- | Tool data type.
-- Used for tool changes, radius compensation.
data Tool = 
        EndMill { diameter :: Double, len :: Double }
        | BallEndMill { diameter :: Double, shankDiameter :: Double, len :: Double } -- ^ The coordinates of the tool are the center of the ball (and not the bottom of the tool)
        | ProbeTool { diameter :: Double, len :: Double }
        deriving (Eq, Show)

data ArcDirection =     CW      -- ^ Clockwise
                        | CCW   -- ^ Counterclockwise
                        deriving (Eq, Show)


data CuttingParameters = CuttingParameters {
        transformation :: Transformation
        , feedRate :: Double
        , plungeRate :: Double
        , probeRate :: Double
        , depthOfCut :: Double  -- ^ Must be a negative number
        , initialPosition :: V3 Double
        , initialTool :: Tool
        }
