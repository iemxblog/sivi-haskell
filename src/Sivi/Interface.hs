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
import Sivi.Interface.ProgramThread
import Sivi.GCode

data Input = 	XMinus | XPlus | YMinus | YPlus | ZMinus | ZPlus 
		| StartProgram | PauseProgram | StopProgram
		| Exit
		deriving (Eq, Show)

initInterface :: IO ()
initInterface = do
	hSetEcho stdin False
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	clearScreen
	hideCursor
	setTitle "Sivi"

exitInterface :: IO ()
exitInterface = do
	clearScreen
	setCursorPosition 0 0
	showCursor

getInput :: IO Input
getInput = do
	char <- getChar
	case char of
		'h' -> return XMinus
		'j' -> return YMinus
		'k' -> return YPlus
		'l' -> return XPlus
		'b' -> return ZMinus
		'n' -> return ZPlus
		's' -> return StartProgram
		'p' -> return PauseProgram
		'd' -> return StopProgram
		'q' -> return Exit	
		_ -> getInput

loop :: Chan WriteCommand -> Chan ReadCommand -> Chan ProgramThreadCommand -> IO ()
loop wc rc ptc = do
	i <- getInput
	case i of
		StartProgram -> writeChan ptc Start >> loop wc rc ptc
		PauseProgram -> writeChan ptc Pause >> loop wc rc ptc
		StopProgram -> writeChan ptc Stop >> loop wc rc ptc
		XMinus -> sendProgram wc rc "G91 G00 X-1" >> sendProgram wc rc "G90" >> loop wc rc ptc
		YMinus -> sendProgram wc rc "G91 G00 Y-1" >> sendProgram wc rc "G90" >> loop wc rc ptc 
		YPlus -> sendProgram wc rc "G91 G00 Y1" >> sendProgram wc rc "G90" >> loop wc rc ptc 
		XPlus -> sendProgram wc rc "G91 G00 X1" >> sendProgram wc rc "G90" >> loop wc rc ptc
		ZMinus -> sendProgram wc rc "G91 G00 Z-1" >> sendProgram wc rc "G90" >> loop wc rc ptc
		ZPlus -> sendProgram wc rc "G91 G00 Z1" >> sendProgram wc rc "G90" >> loop wc rc ptc
		Exit -> return ()

-- | Text mode interface for GRBL.
interface :: GCode -> IO ()
interface gcode = do
	let port = "/dev/ttyACM0"
	serial <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
	initInterface 
	putStrLn "Serial port opened."
	rc <- newChan
	wc <- newChan
	pc <- newChan
	ptc <- newChan
	rc2 <- dupChan rc
	forkIO $ printerThread pc 
	forkIO $ serialReader rc pc serial []
	forkIO $ serialWriter wc pc serial 
	forkIO $ toolPositionThread wc rc pc 
	forkIO $ programThread wc rc2 pc ptc gcode Paused
	loop wc rc2 ptc
	closeSerial serial
	exitInterface
