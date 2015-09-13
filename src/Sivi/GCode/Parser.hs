{-|
Module		: Sivi.GCode.Parser
Description	: GCode parser
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Sivi.GCode.Parser
(
	parse
) where

import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Control.Applicative
import qualified Data.ByteString as B
import Data.List
import Sivi.GCode.Base

-- | Parses a GCode word
word :: Char 				-- ^ wn : The name of the word
	-> Parser (Maybe Double)	-- ^ The resulting parser
word wn = do
		char wn
		d <- double
		return $ Just d
	  <|>
		pure Nothing

-- | Skips spaces before and after
token :: Parser a 			-- ^ p : The parser to tokenize
	-> Parser a			-- ^ The resulting parser
token p = many (satisfy (==' ')) *> p <* many (satisfy (==' '))

-- | Skips spaces before and after a string
symbol :: B.ByteString -> Parser B.ByteString
symbol xs = token (string xs)

-- | Parses parameters of a GCode command
pParams :: [Char] 			-- ^ List of parameter names
	-> Parser [Maybe Double]	-- ^ Resulting parser
pParams = mapM (token . word) 

-- | Parses a G00 (rapid move)
pG00 :: Parser GCode
pG00 = do 
		symbol "G00"
		[x, y, z] <- pParams "XYZ"
		return $ G00 x y z

-- | Parses a G01 (linear interpolation)
pG01 :: Parser GCode
pG01 = do
		symbol "G01"
		[x, y, z, f] <- pParams "XYZF"
		return $ G01 x y z f

-- | Parses a G02 (clockwise circular interpolation)
pG02 :: Parser GCode
pG02 = do
		symbol "G02"
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ G02 x y z i j k f

-- | Parses a G03 (counter-clockwise circular interpolation)
pG03 :: Parser GCode
pG03 = do
		symbol "G03"
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ G03 x y z i j k f

-- | Parses a comment
pComment :: Parser GCode
pComment = do
		symbol "(" 
		c <- many (satisfy (/=')')) 
		symbol ")"
		return $ GComment (dropWhileEnd isSpace c)

-- | Parses a M00 (pause)
pM00 :: Parser GCode
pM00 = symbol "M00" >> return M00

-- | Parses a G38.2 (probe)
pG38d2 :: Parser GCode
pG38d2 = do
		symbol "G38.2"
		[x, y, z, f] <- pParams "XYZF"
		return $ G38d2 x y z f

pG92 :: Parser GCode
pG92 = do
		symbol "G92"
		[x, y, z] <- pParams "XYZ"
		return $ G92 x y z

-- | Parses a line with only parameters, and no command
pCLine :: Parser GCode
pCLine = do
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ CLine x y z i j k f

-- | Parses a GCode command
pGCode :: Parser GCode
pGCode = pG00 <|> pG01 <|> pG02 <|> pG03 <|> pComment <|> pM00 <|> pG38d2 <|> pG92 <|> pCLine 

-- | Parses a GCode program (list of commands)
pProgram :: Parser [GCode]
pProgram = pGCode `sepBy` endOfLine

-- | Parses a GCode program
parse :: B.ByteString -> Either String [GCode]
parse = parseOnly pProgram
