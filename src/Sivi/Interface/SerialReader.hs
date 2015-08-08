{-|
Module		: Sivi.Interface.SerialReader
Description	: Thread for serial port reading
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.SerialReader
(
	serialReader
	, ReadCommand(..)
) where

import Control.Concurrent.Chan
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B

data ReadCommand = Position (Double, Double, Double) | Ok deriving (Eq, Show)

-- | Takes a string as parameter, and returns a tuple of the first line and the remaining of the string.
splitLine :: 	String
		-> (String, String)
splitLine xs | not ('\n' `elem` xs) = ([], xs)
splitLine xs = mapSnd tail . break (=='\n') $ xs
	where mapSnd f (a, b) = (a, f b)

serialReader :: Chan String -> SerialPort -> String -> IO () 
serialReader chan serial buffer = do
	msg <- recv serial 100
	let msgString = B.unpack msg
	let (newMsg, newBuf) = splitLine (buffer ++ msgString)
	if length newMsg > 0 then
		do 
			writeChan chan newMsg
			putStrLn $ "Received : " ++ newMsg
	else
		return ()
	serialReader chan serial newBuf -- looping recursively

