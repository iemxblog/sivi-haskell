{-|
Module		: Sivi.Interface.PrinterThread
Description	: Thread for doing IO actions
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.PrinterThread
(
	printerThread
) where
import Control.Concurrent.Chan
import Control.Monad(forever)

printerThread :: Chan (IO ()) -> IO()
printerThread pc = forever $ do
	action <- readChan pc
	action
	
