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

data WriteCommand = GetPosition | GCode String deriving Eq

instance Show WriteCommand where
	show GetPosition = "?"
	show (GCode s) = s

serialWriter :: Chan String -> SerialPort -> IO ()
serialWriter chan serial = forever $ do
	msg <- readChan chan
	send serial (B.pack msg)
	putStrLn $ "Sent : " ++ msg

