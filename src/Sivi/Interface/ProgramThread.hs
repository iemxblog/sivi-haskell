{-|
Module		: Sivi.Interface.ProgramThread
Description	: Thread that sends the GCode program over the serial port
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.ProgramThread
(
	ThreadState(..)
	, ProgramThreadCommand(..)
	, programThread	
) where

import Control.Concurrent
import System.Console.ANSI
import Sivi.GCode
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader

data ThreadState = Running | Paused
data ProgramThreadCommand = Start | Pause | Stop

programThread :: Chan WriteCommand -> Chan ReadCommand -> Chan (IO ()) -> Chan ProgramThreadCommand -> [GCode] -> ThreadState -> IO ()
programThread wc rc pc ptc xs Paused = do
	ptcommand <- readChan ptc
	case ptcommand of
		Start -> programThread wc rc pc ptc xs Running
		Pause -> programThread wc rc pc ptc xs Paused
		Stop -> return ()
programThread wc rc pc ptc (x:xs) Running = do	
	iec <- isEmptyChan ptc	
	if not iec then
		programThread wc rc pc ptc (x:xs) Paused
	else do
		writeChan pc (setCursorPosition 0 0)
		writeChan pc (putStr "   -> ")
		writeChan pc (putStr $ show x)
		writeChan pc (setCursorPosition 1 0)
		writeChan pc (mapM_ print $ take 10 xs)
		case x of
			GComment s -> writeChan pc (setCursorPosition 24 0 >> putStrLn s)
			M00 -> programThread wc rc pc ptc xs Paused
			_ -> do 
				writeChan wc (SendProgram $ show x) 
				waitFor rc Ok 
				writeChan pc (setCursorPosition 0 0)
				writeChan pc (putStr "ok")
		--threadDelay (10^6*2)
		programThread wc rc pc ptc xs Running
