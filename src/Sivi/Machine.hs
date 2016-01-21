{-|
Module          : Sivi.Machine
Description     : Machine-specific functions
Copyright       : (c) Maxime ANDRE, 2016
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Machine
(
        Machine(..)
) where

import Sivi.Backend
import Sivi.Operation.Types

-- | Machine type class,
-- used to define functions that are specific to a machine. 
-- For example, the procedure is not the same to change a tool on 2 different machines.
class Machine m where
    changeTool :: Backend w => Tool -> Operation m w ()     -- ^ Do not forget to call 'putTool' in this function definition.
