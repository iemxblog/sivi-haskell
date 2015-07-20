{-|
Module		: GCode.Parser
Description	: GCode parser
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module GCode.Parser
(
	parse
) where

import Data.Attoparsec.ByteString.Char8 hiding (parse)
import Control.Applicative
import qualified Data.ByteString as B
import Data.List
import GCode.Base

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
symbol xs = token (string xs)

-- | Parses parameters of a GCode command
pParams :: [Char] 			-- ^ List of parameter names
	-> Parser [Maybe Double]	-- ^ Resulting parser
pParams = sequence . map (\c -> token $ word c) 

-- | Parses a G00 (rapid move)
pG00 :: Parser GCode
pG00 = do 
		symbol "G00"
		[x, y, z] <- pParams ['X', 'Y', 'Z']
		return $ G00 x y z

-- | Parses a G01 (linear interpolation)
pG01 :: Parser GCode
pG01 = do
		symbol "G01"
		[x, y, z, f] <- pParams ['X', 'Y', 'Z', 'F']
		return $ G01 x y z f

-- | Parses a G02 (clockwise circular interpolation)
pG02 :: Parser GCode
pG02 = do
		symbol "G02"
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ G02 x y z i j k f

-- | Parses a G03 (counter-clockwise circular interpolation)
pG03 :: Parser GCode
pG03 = do
		symbol "G03"
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ G03 x y z i j k f

-- | Parses a M06 (tool change)
pM06 :: Parser GCode
pM06 = symbol "M06" >> token (char 'T' >> many1 digit) >>= \tn -> return (M06 tn)

-- | Parses a comment
pComment :: Parser GCode
pComment = do
		symbol "(" 
		c <- many (satisfy (/=')')) 
		symbol ")"
		return $ Comment (dropWhileEnd isSpace c)

-- | Parses a M00 (pause)
pM00 :: Parser GCode
pM00 = symbol "M00" >> return M00

-- | Parses a line with only parameters, and no command
pCLine = do
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ CLine x y z i j k f

-- | Parses a GCode command
pGCode = pG00 <|> pG01 <|> pG02 <|> pG03 <|> pM06 <|> pComment <|> pM00 <|> pCLine

-- | Parses a GCode program (list of commands)
pProgram :: Parser [GCode]
pProgram = pGCode `sepBy` endOfLine

-- | Parses a GCode program
parse :: B.ByteString -> Either String [GCode]
parse = parseOnly pProgram
