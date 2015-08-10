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

interface :: IO ()
interface = do
	let port = "/dev/ttyACM0"
	s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
	initInterface 
	putStrLn "Serial port opened."
	rc <- newChan
	wc <- newChan
	pc <- newChan
	forkIO $ printerThread pc 
	forkIO $ serialReader rc pc s []
	forkIO $ serialWriter wc pc s 
	forkIO $ toolPositionThread wc rc pc 
	loop s
	closeSerial s
	exitInterface
