{-|
Module		: Sivi.Interface.ToolPositionThread
Description	: Thread that prints the tool position
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Interface.ToolPositionThread
(
	toolPositionThread
) where

import Control.Concurrent.Chan
import Sivi.Interface.SerialWriter
import Sivi.Interface.SerialReader
import Control.Monad(forever)

toolPositionThread :: Chan WriteCommand -> Chan ReadCommand -> IO ()
toolPositionThread wc rc = forever $ do
	writeChan wc GetPosition
	msg <- readChan rc
	case msg of
		Position (x, y, z) -> putStrLn $ "X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z
		_ -> return ()
	toolPositionThread wc rc
