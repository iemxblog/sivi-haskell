{-|
Module          : Sivi.Interface.SerialReader
Description     : Thread for serial port reading
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Interface.SerialReader
(
        serialReader
        , ReadCommand(..)
        , waitFor
) where

import Control.Concurrent.Chan
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Control.Monad
import System.Console.ANSI
import Text.Parsec hiding (Ok)
import Sivi.Interface.Misc
import Sivi.Misc.SharedParsers

-- | Data returned by the serialRead thread. The data constructors represent GRBL response messages.
data ReadCommand = Position (Double, Double, Double) | Ok deriving (Eq, Show)

-- | Parses a position message and returns the current working position.
pPosition :: Parsec String () ReadCommand
pPosition = do
        char '<'
        many1 (satisfy (/=','))
        char ','
        string "MPos:"
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

-- | Parses ok messages.
pOk :: Parsec String () ReadCommand
pOk = string "ok" >> return Ok

-- | Parses GRBL response messages.
pReadCommand :: Parsec String () ReadCommand
pReadCommand = pPosition <|> pOk

-- | Takes a 'String' as parameter, and returns a tuple of the first line and the remaining of the string.
splitLine ::    String
                -> (String, String)
splitLine xs | '\n' `notElem` xs = ([], xs)
splitLine xs = mapSnd tail . break (=='\n') $ xs
        where mapSnd f (a, b) = (a, f b)

-- | Waits for data coming from the serial port, and sends it as 'ReadCommand' on a Chan.
serialReader :: Chan ReadCommand        -- ^ rc : The Chan where the commands are sent
                -> Chan (IO ())         -- ^ pc : The Chan to send IO Commands (putStrLn, ...) to the 'Sivi.Interface.PrinterThread.printerThread'
                -> SerialPort           -- ^ serial : The serial port
                -> String               -- ^ buffer : The initial buffer contents -> [] (empty list). Necessary because of recursivity.
                -> IO () 
serialReader rc pc serial buffer = do
        msg <- recv serial 100
        let msgString = B.unpack msg
        let (newMsg, newBuf) = splitLine (buffer ++ msgString)
        unless (null newMsg) $
                case parse pReadCommand "(grbl commands)" newMsg of
                        Left _ -> writeChan pc $ do
                                setCursorPosition 5 60
                                withColor Red (putStr $ showLine 20 newMsg)
                        Right readCommand -> writeChan rc readCommand -- >> writeChan pc (putStrLn ("Received : " ++ show readCommand))
        serialReader rc pc serial newBuf -- looping recursively

waitFor :: Chan ReadCommand -> ReadCommand -> IO ()
waitFor rc readcommand = do
        msg <- readChan rc
        unless (msg == readcommand) $ waitFor rc readcommand
