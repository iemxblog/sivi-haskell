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
	, getCurrentPosition
) where

import System.Console.ANSI
import Control.Concurrent
import Control.Monad(forever)
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader
import Sivi.Interface.Misc

-- | This thread sends commands to GRBL to get the current working position, then waits for the response and prints it at some location on the screen.
toolPositionThread :: 	Chan WriteCommand 	-- ^ The channel communicating with serialWriter (to send data to the serial port)
			-> Chan ReadCommand 	-- ^ The channel communicating with serialReader (to read dat a from the serial port)
			-> Chan (IO ()) 	-- ^ The channel communicating with printerThread (to perform IO actions safely)
			-> IO ()
toolPositionThread wc rc pc = forever $ do
	(x, y, z) <- getCurrentPosition wc rc
	writeChan pc 	(withColor Blue $ do
				setCursorPosition 1 60  
				putStr . showLine 8 $ "X" ++ show x
				setCursorPosition 2 60  
				putStr . showLine 8 $ "Y" ++ show y
				setCursorPosition 3 60  
				putStr . showLine 8 $ "Z" ++ show z
			)
	threadDelay (10^5)

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

