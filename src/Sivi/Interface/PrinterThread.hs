{-|
Module          : Sivi.Interface.PrinterThread
Description     : Thread for doing IO actions
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Interface.PrinterThread
(
        printerThread
) where
import Control.Concurrent.Chan
import Control.Monad(forever, join)

-- | This thread performs IO actions coming from a Chan. Used to atomically do those actions. If we don't print with it, a thread prints some characters, then the following thread prints some characters, etc.
printerThread :: Chan (IO ()) -> IO()
printerThread pc = forever $ join (readChan pc)
