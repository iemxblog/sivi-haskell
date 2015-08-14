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
import Sivi.Interface.Misc

data ThreadState = Running | Paused
data ProgramThreadCommand = Start | Pause | Stop

putInstruction :: String -> IO ()
putInstruction s = setCursorPosition 24 0 >> putStrLn (showLine 80 s)

clearInstruction :: IO ()
clearInstruction = setCursorPosition 24 0 >> putStrLn (replicate 80 ' ')


programThread :: Chan WriteCommand -> Chan ReadCommand -> Chan (IO ()) -> Chan ProgramThreadCommand -> [GCode] -> ThreadState -> IO ()
programThread _ _ pc _ [] _ = do
	writeChan pc $ do
		setCursorPosition 0 0
		withColor Green $ mapM_ putStr (showLines 58 22 ["End."])
	threadDelay (10^7)
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
		writeChan pc (putStr $ showLine 52 (show x))
		writeChan pc (setCursorPosition 1 0)
		writeChan pc (mapM_ putStrLn $ showLines 58 22 (map show xs))
		case x of
			GComment s -> do
				writeChan pc (withColor Green $ putInstruction s)
				programThread wc rc pc ptc xs Running
			M00 -> programThread wc rc pc ptc xs Paused
			_ -> do 
				writeChan wc (SendProgram $ show x) 
				waitFor rc Ok 
				writeChan pc (setCursorPosition 0 0)
				writeChan pc (putStr "ok")
				writeChan pc clearInstruction
				programThread wc rc pc ptc xs Running
