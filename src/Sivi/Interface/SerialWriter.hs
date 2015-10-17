{-|
Module		: Sivi.Interface.SerialWriter
Description	: Thread for serial port writing
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.SerialWriter
(
	serialWriter
	, WriteCommand(..)
	, sendProgram
) where

import Control.Concurrent.Chan
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Sivi.Interface.SerialReader

-- | Commands sent to the 'serialWriter' thread. The data constructors represent GRBL commands.
data WriteCommand = GetPosition | SendProgram String deriving (Eq, Show)

-- | Transforms a 'WriteCommand' data to a GRBL command.
toGRBLCommand :: WriteCommand -> String
toGRBLCommand GetPosition = "?"
toGRBLCommand (SendProgram s) = s ++ "\n"

sendProgram :: Chan WriteCommand -> Chan ReadCommand -> String -> IO ()
sendProgram wc rc s = writeChan wc (SendProgram s) >> waitFor rc Ok

-- | Waits for commands on a Chan, and sends them over the serial port. To be used with 'forkIO'.
serialWriter :: Chan WriteCommand 	-- ^ wc : The Chan where the incoming commands come from
		-> Chan (IO ()) 	-- ^ pc : The Chan to send IO commands (putStrLn, ...) to the 'Sivi.Interface.PrinterThread.printerThread'
		-> SerialPort 		-- ^ serial : The serial port
		-> IO ()
serialWriter wc _ serial = forever $ do
	writeCommand <- readChan wc
	let grblCommand = toGRBLCommand writeCommand
	bytes <- send serial (B.pack grblCommand)
	when (bytes /= length grblCommand) $ error "serialWriter : 'send' did not send all the data. Better 'send' not implemented yet."
	--writeChan pc (putStrLn $ "Sent : " ++ show writeCommand)

