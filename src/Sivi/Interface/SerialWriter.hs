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
) where

import Control.Concurrent
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Control.Monad(forever)
import Data.Char

data WriteCommand = GetPosition | GCode String deriving (Eq, Show)

toGRBLCommand :: WriteCommand -> String
toGRBLCommand GetPosition = "?"
toGRBLCommand (GCode s) = s ++ "\n"

serialWriter :: Chan WriteCommand -> SerialPort -> Chan (IO ()) -> IO ()
serialWriter chan serial pc = forever $ do
	wc <- readChan chan
	send serial (B.pack (toGRBLCommand wc))
	writeChan pc (putStrLn $ "Sent : " ++ show wc)

