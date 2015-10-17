{-|
Module		: Sivi.Operation.Types
Description	: Type declarations
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Types
(
	Transformation
	, Operation
	, Tool(..)
	, ArcDirection(..)
) where

import Linear
import Control.Monad.State
import Control.Monad.Reader

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
-- * Double (in the State Monad) : The current machine position
--
-- * Tool (in the State Monad) : The current tool
type Operation a = ReaderT (Transformation, Double, Double, Double, Double) (State (V3 Double, Tool)) a


-- | Tool data type.
-- Used for tool changes, radius compensation.
data Tool = 
	EndMill { diameter :: Double, len :: Double }
	| BallEndMill { diameter :: Double, shankDiameter :: Double, len :: Double } 
	| ProbeTool { diameter :: Double, len :: Double }
	deriving (Eq, Show)

data ArcDirection = 	CW 	-- ^ Clockwise
			| CCW 	-- ^ Counterclockwise
			deriving (Eq, Show)

