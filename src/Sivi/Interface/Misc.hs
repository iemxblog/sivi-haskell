{-|
Module		: Sivi.Interface.Misc
Description	: Miscellaneous functions
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.Misc
(
	getCurrentPosition
	, withColor
	, showLine
	, showLines
) where

import Control.Concurrent.Chan
import System.Console.ANSI
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader

waitPosition :: Chan ReadCommand -> IO (Double, Double, Double)
waitPosition rc = do
	readCommand <- readChan rc
	case readCommand of
		Position (x, y, z) -> return (x, y, z)	
		_ -> waitPosition rc

getCurrentPosition :: Chan WriteCommand -> Chan ReadCommand -> IO (Double, Double, Double)
getCurrentPosition wc rc = do
	writeChan wc GetPosition
	waitPosition rc

withColor :: Color -> IO () -> IO ()
withColor c a = do 
	setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
	a
	setSGR [ SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White ]

cutFill :: Int -> a -> [a] -> [a]
cutFill n x xs = take n $ xs ++ repeat x

showLine :: Int -> String -> String
showLine w xs = cutFill w ' ' xs

showLines :: Int -> Int -> [String] -> [String]
showLines w h xs = cutFill h (replicate w ' ') (map (showLine w) xs)

