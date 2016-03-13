{-|
Module          : Sivi.Operation.Run
Description     : Run an operation
Copyright       : (c) Maxime ANDRE, 2016
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}

module Sivi.Operation.Run (
    runOperation
    , getReturnValue
    , defaultCuttingParameters
) where

import Sivi.Machine
import Sivi.MachineInstances
import Sivi.Operation.Types
import Sivi.Backend
import Control.Monad.RWS
import Linear

-- | Runs an operation with the specified parameters.
runOperation :: (Machine m, Backend w) => 
                CuttingParameters m          -- ^ Cutting parameters
                -> Operation m w ()             -- ^ op : Operation to run
                -> w 
runOperation cpar op = w
    where 
        (_, _, w) = runRWS op env st
        env = buildEnvironment cpar
        st = buildState cpar

-- | Runs an operation and returns the return value. Used in tests.
getReturnValue ::   Machine m => 
                    CuttingParameters m
                    -> Operation m w a 
                    -> a
getReturnValue cpar op = a
    where
        a = fst $ evalRWS op env st
        env = buildEnvironment cpar
        st = buildState cpar
        
-- | Default cutting parameters.
defaultCuttingParameters :: CuttingParameters MF70
defaultCuttingParameters = CuttingParameters {transformation = id, feedRate = 100, plungeRate = 30, probeRate = 10, depthOfCut = -0.5, machine = MF70, initialPosition = V3 0 0 50, initialTool = EndMill{diameter = 3, len=42}}

