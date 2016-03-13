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
        , OpEnvironment(..)
        , OpState(..)
        , Tool(..)
        , ArcDirection(..)
        , CuttingParameters(..)
        , buildEnvironment
        , buildState
) where

import Linear
import Control.Monad.RWS

type Transformation = V3 Double -> V3 Double

-- | The Operation type.
type Operation m w a = RWS (OpEnvironment m) w OpState a


-- | Contains the Reader part of the 'Operation' monad.
-- Before, a tuple was used. But a custom datatype is more readable, and it is easier to add new parameters.
-- Each parameter name is prepended with an "e" to avoid name collisions with functions like 'Sivi.Operation.Base.getTransformation', 'Sivi.Operation.Base.getFeedRate', ... Moreover, these names are used only internally, and won't be used by the user. They are a bit ugly, but it's not really a problem.
data OpEnvironment m = OpEnvironment {
    eTransformation :: Transformation 
    , eFeedRate :: Double 
    , ePlungeRate :: Double 
    , eProbeRate :: Double 
    , eDepthOfCut :: Double 
    , eMachine :: m
    }

-- | Contains the State part of the 'Operation' monad.
-- Each parameter is prepended with an "s" tto avoid name collisions. For the same reason as 'OpEnvironment'.
data OpState = OpState {
    sCurrentPosition :: (V3 Double) 
    , sTool :: Tool
    }

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

-- | Cutting parameters : contains the feed rate, plunge rate, etc. They are needed when we want to run an operation with 'Sivi.Operation.Run.runOperation'.
data CuttingParameters m = CuttingParameters {
        transformation :: Transformation    -- ^ the initial transformation should always be 'Prelude.id' (identity function, i.e. no transformation). Use something else if you know what you are doing.
        , feedRate :: Double
        , plungeRate :: Double
        , probeRate :: Double
        , depthOfCut :: Double  -- ^ Must be a negative number
        , machine :: m
        , initialPosition :: V3 Double
        , initialTool :: Tool
        }

-- | Gets the environment part of the cutting parameters.
buildEnvironment :: CuttingParameters m -> OpEnvironment m
buildEnvironment (CuttingParameters tr fr pr pbr dc m _ _) = OpEnvironment tr fr pr pbr dc m

-- | Gets the state part of the cutting parameters.
buildState :: CuttingParameters m -> OpState
buildState (CuttingParameters _ _ _ _ _ _ ipos itool) = OpState ipos itool
