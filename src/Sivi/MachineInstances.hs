{-|
Module          : Sivi.MachineInstances
Description     : Implementation of some machine instances
Copyright       : (c) Maxime ANDRE, 2016
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.MachineInstances
(
    MF70(..)
) where

import Sivi.Machine
import Sivi.Operation.Base


data MF70 = MF70

instance Machine MF70 where
    changeTool t = do
        retract 20 
        message ("Please place the tool " ++ show t ++ " in the spindle.")
        putTool t
