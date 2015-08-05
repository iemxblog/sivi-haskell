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
	--clearScreen
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

loop = do
	i <- getInput
	case i of
		Exit -> return ()
		_ -> putStrLn (show i) >> loop	

counter n = do
	setCursorPosition 10 10
	putStrLn $ show n
	threadDelay (10^6)
	counter (n+1)

interface :: IO ()
interface = do
	initInterface 
	forkIO $ counter 0
	loop
	exitInterface
