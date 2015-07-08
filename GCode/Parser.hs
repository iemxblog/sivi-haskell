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
	parser
) where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List
import GCode.Base

word :: Char -> Parser (Maybe Double)
word wn = do
		char wn
		d <- double
		return $ Just d
	  <|>
		pure Nothing

token :: Parser a -> Parser a
token p = many (satisfy (==' ')) *> p <* many (satisfy (==' '))

symbol xs = token (string xs)

pParams :: [Char] -> Parser [Maybe Double]
pParams = sequence . map (\c -> token $ word c) 

pG00 :: Parser GCode
pG00 = do 
		symbol "G00"
		[x, y, z] <- pParams ['X', 'Y', 'Z']
		return $ G00 x y z

pG01 :: Parser GCode
pG01 = do
		symbol "G01"
		[x, y, z, f] <- pParams ['X', 'Y', 'Z', 'F']
		return $ G01 x y z f

pG02 :: Parser GCode
pG02 = do
		symbol "G02"
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ G02 x y z i j k f

pG03 :: Parser GCode
pG03 = do
		symbol "G03"
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ G03 x y z i j k f

pM06 :: Parser GCode
pM06 = symbol "M06" >> token (char 'T' >> many1 digit) >>= \tn -> return (M06 tn)

pComment :: Parser GCode
pComment = do
		symbol "(" 
		c <- many (satisfy (/=')')) 
		symbol ")"
		return $ Comment (dropWhileEnd isSpace c)

pM00 :: Parser GCode
pM00 = symbol "M00" >> return M00

pCLine = do
		[x, y, z, i, j, k, f] <- pParams ['X', 'Y', 'Z', 'I', 'J', 'K', 'F']
		return $ CLine x y z i j k f

pGCode = pG00 <|> pG01 <|> pG02 <|> pG03 <|> pM06 <|> pComment <|> pM00 <|> pCLine

pProgram :: Parser [GCode]
pProgram = pGCode `sepBy` endOfLine

parser :: Parser [GCode]
parser = pProgram
		
--test = parseOnly (pParams ['X', 'Y', 'Z']) "X4  Y3    Z5"
--test2 = parseOnly pProgram "G03 X1 Y2 I-2\nM00\nM06 T01\n( Commentaire )\nG01 X0 Z0 F10\nX1 Z3\nG00 Z10"
