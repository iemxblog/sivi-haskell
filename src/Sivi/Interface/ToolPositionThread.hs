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
	writeChan pc (setCursorPosition 1 60 >> (putStrLn $ "X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z))
	threadDelay (10^5)
