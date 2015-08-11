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

-- | This thread sends commands to GRBL to get the current working position, then waits for the response and prints it at some location on the screen.
toolPositionThread :: 	Chan WriteCommand 	-- ^ The channel communicating with serialWriter (to send data to the serial port)
			-> Chan ReadCommand 	-- ^ The channel communicating with serialReader (to read dat a from the serial port)
			-> Chan (IO ()) 	-- ^ The channel communicating with printerThread (to perform IO actions safely)
			-> IO ()
toolPositionThread wc rc pc = forever $ do
	writeChan wc GetPosition
	readCommand <- readChan rc
	case readCommand of
		Position (x, y, z) -> writeChan pc (setCursorPosition 1 60 >> (putStrLn $ "X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z))
		_ -> return ()
	threadDelay (10^6)
	toolPositionThread wc rc pc
