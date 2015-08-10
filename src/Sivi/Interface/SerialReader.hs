{-|
Module		: Sivi.Interface.SerialReader
Description	: Thread for serial port reading
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Sivi.Interface.SerialReader
(
	serialReader
	, ReadCommand(..)
) where

import Control.Concurrent.Chan
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

data ReadCommand = Position (Double, Double, Double) | Ok deriving (Eq, Show)

-- ##########################################################################
-- Parsers
pPosition :: Parser ReadCommand
pPosition = do
	string "<Idle,MPos:"
	double
	char ','
	double
	char ','
	double
	string ",WPos:"
	x <- double
	char ','
	y <- double
	char ','
	z <- double
	char '>'
	return $ Position (x, y, z)

pOk :: Parser ReadCommand
pOk = string "ok" >> return Ok

pReadCommand :: Parser ReadCommand
pReadCommand = pPosition <|> pOk

-- ##########################################################################

-- | Takes a string as parameter, and returns a tuple of the first line and the remaining of the string.
splitLine :: 	String
		-> (String, String)
splitLine xs | not ('\n' `elem` xs) = ([], xs)
splitLine xs = mapSnd tail . break (=='\n') $ xs
	where mapSnd f (a, b) = (a, f b)

serialReader :: Chan ReadCommand -> Chan (IO ()) -> SerialPort -> String -> IO () 
serialReader rc pc serial buffer = do
	msg <- recv serial 100
	let msgString = B.unpack msg
	let (newMsg, newBuf) = splitLine (buffer ++ msgString)
	if length newMsg > 0 then
		case parseOnly pReadCommand (B.pack newMsg) of
			Left err -> return ()
			Right readCommand -> writeChan rc readCommand >> writeChan pc (putStrLn ("Received : " ++ show readCommand))
	else
		return ()
	serialReader rc pc serial newBuf -- looping recursively

