{-|
Module		: Sivi.Interface
Description	: Provides a text mode interface to send programs to GRBL
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface
(
	interface
) where

import System.Console.ANSI
import System.IO
import Control.Concurrent
import System.Hardware.Serialport
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader
import Sivi.Interface.ToolPositionThread
import Sivi.Interface.PrinterThread

data Input = XMinus | XPlus | YMinus | YPlus | ZMinus | ZPlus | Exit deriving (Eq, Show)

initInterface :: IO ()
initInterface = do
	hSetEcho stdin False
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	clearScreen
	setTitle "Sivi"

exitInterface :: IO ()
exitInterface = do
	clearScreen
	setCursorPosition 0 0
	showCursor

getInput = do
	char <- getChar
	case char of
		'h' -> return XMinus
		'j' -> return YMinus
		'k' -> return YPlus
		'l' -> return XPlus
		'b' -> return ZMinus
		'n' -> return ZPlus
		'q' -> return Exit	
		_ -> getInput

loop s = do
	i <- getInput
	case i of
		Exit -> return ()
		_ -> putStrLn (show i) >> loop s	

-- | Text mode interface for GRBL.
interface :: IO ()
interface = do
	let port = "/dev/ttyACM0"
	serial <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
	initInterface 
	putStrLn "Serial port opened."
	rc <- newChan
	wc <- newChan
	pc <- newChan
	forkIO $ printerThread pc 
	forkIO $ serialReader rc pc serial []
	forkIO $ serialWriter wc pc serial 
	forkIO $ toolPositionThread wc rc pc 
	loop serial
	closeSerial serial
	exitInterface
