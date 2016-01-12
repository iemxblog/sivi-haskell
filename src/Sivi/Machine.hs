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

class Machine m where
    changeTool :: Backend w => Tool -> Operation m w ()

