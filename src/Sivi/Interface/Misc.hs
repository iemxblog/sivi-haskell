{-|
Module		: Sivi.Interface.Misc
Description	: Miscellaneous functions
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.Misc
(
	getCurrentPosition
) where

import Control.Concurrent.Chan
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader

waitPosition :: Chan ReadCommand -> IO (Double, Double, Double)
waitPosition rc = do
	readCommand <- readChan rc
	case readCommand of
		Position (x, y, z) -> return (x, y, z)	
		_ -> waitPosition rc

getCurrentPosition :: Chan WriteCommand -> Chan ReadCommand -> IO (Double, Double, Double)
getCurrentPosition wc rc = do
	writeChan wc GetPosition
	waitPosition rc
