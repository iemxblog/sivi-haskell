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
import qualified Data.ByteString.Char8 as B
import Control.Monad(forever)

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

counter n = do
	setCursorPosition 10 10
	putStrLn $ show n
	threadDelay (10^6)
	counter (n+1)

serialReader :: Chan String  -> SerialPort -> IO () 
serialReader chan serial = forever $ do
	msg <- recv serial 100
	let msgString = B.unpack msg
	if length msgString > 0 then
		do 
			writeChan chan msgString
			putStrLn $ "Received : " ++ msgString
	else
		return ()
	 

serialWriter :: Chan String -> SerialPort -> IO ()
serialWriter chan serial = forever $ do
	msg <- readChan chan
	send serial (B.pack msg)
	putStrLn $ "Sent : " ++ msg
	
interface :: IO ()
interface = do
	let port = "/dev/ttyACM0"
	s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }
	initInterface 
	putStrLn "Serial port opened."
	chanRead <- newChan
	chanWrite <- newChan
	forkIO $ counter 0
	forkIO $ serialReader chanRead s
	forkIO $ serialWriter chanWrite s
	loop s
	closeSerial s
	exitInterface
