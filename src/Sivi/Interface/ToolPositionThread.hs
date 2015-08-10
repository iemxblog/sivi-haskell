{-|
Module		: Sivi.Interface.ToolPositionThread
Description	: Thread that prints the tool position
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.ToolPositionThread
(
	toolPositionThread
) where

import System.Console.ANSI
import Control.Concurrent
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader
import Control.Monad(forever)

toolPositionThread :: Chan WriteCommand -> Chan ReadCommand -> Chan (IO ()) -> IO ()
toolPositionThread wc rc pc = forever $ do
	writeChan wc GetPosition
	readCommand <- readChan rc
	case readCommand of
		Position (x, y, z) -> writeChan pc (setCursorPosition 1 60 >> (putStrLn $ "X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z))
		_ -> return ()
	threadDelay (10^6)
	toolPositionThread wc rc pc
