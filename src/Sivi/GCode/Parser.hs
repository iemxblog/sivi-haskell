{-|
Module		: Sivi.GCode.Parser
Description	: GCode parser
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.GCode.Parser
(
	parseGCode
) where

import Text.Parsec
import Text.Parsec ((<?>))
import Sivi.GCode.Base
import Control.Applicative((<*), (*>))
import Data.Char

double :: Parsec String () Double
double = do
		sign <- option "" (string "-")
		integerPart <- many1 (satisfy isDigit)
		decimalPart <- option "" $ do
			p <- char '.' 
			d <- many1 (satisfy isDigit)
			return (p:d)
		return (read (sign++integerPart++decimalPart) :: Double)
			

word :: Char 
	-> Parsec String () (Maybe Double)
word wn = (optionMaybe $ do
		char wn
		double
	   ) <?> "GCode word"

-- | Custom version of 'Text.Parsec.Token.lexeme'. (This custom version doesn't parse newlines).
lexeme :: Parsec String () a -> Parsec String () a
lexeme p = many (satisfy (==' ')) *> p <* many (satisfy (==' '))

symbol :: String -> Parsec String () String
symbol xs = lexeme (string xs)

condition :: (a -> Bool) -> Parsec String () a -> Parsec String () a
condition f p  = do
	r <- p
	case f r of
		True -> return r
		False -> parserFail ""

pParams :: [Char]
	-> Parsec String () [Maybe Double]
pParams =  (condition (not . all (==Nothing))) . mapM (lexeme . word)

-- | Parses a G00 (rapid move)
pG00 :: Parsec String () GCode
pG00 = do 
		symbol "G00"
		[x, y, z] <- pParams "XYZ"
		return $ G00 x y z

-- | Parses a G01 (linear interpolation)
pG01 :: Parsec String () GCode
pG01 = do
		symbol "G01"
		[x, y, z, f] <- pParams "XYZF"
		return $ G01 x y z f

-- | Parses a G02 (clockwise circular interpolation)
pG02 :: Parsec String () GCode
pG02 = do
		symbol "G02"
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ G02 x y z i j k f

-- | Parses a G03 (counter-clockwise circular interpolation)
pG03 :: Parsec String () GCode
pG03 = do
		symbol "G03"
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ G03 x y z i j k f

-- | Parses a comment
pComment :: Parsec String () GCode
pComment = do
		string "("
		c <- manyTill anyChar (try (string ")"))
		return $ GComment c
	     <?> "comment"

-- | Parses a M00 (pause)
pM00 :: Parsec String () GCode
pM00 = symbol "M00" >> return M00

-- | Parses a G38.2 (probe)
pG38d2 :: Parsec String () GCode
pG38d2 = do
		symbol "G38.2"
		[x, y, z, f] <- pParams "XYZF"
		return $ G38d2 x y z f

pG92 :: Parsec String () GCode
pG92 = do
		symbol "G92"
		[x, y, z] <- pParams "XYZ"
		return $ G92 x y z

-- | Parses a line with only parameters, and no command
pCLine :: Parsec String () GCode
pCLine = do
		[x, y, z, i, j, k, f] <- pParams "XYZIJKF"
		return $ CLine x y z i j k f

-- | Parses a GCode command
pGCode :: Parsec String () GCode
pGCode = try pG00 <|> try pG01 <|> try pG02 <|> try pG03 <|> try pComment <|> try pM00 <|> try pG38d2 <|> try pG92 <|> pCLine 

-- | Parses a GCode program (list of commands)
--pProgram :: Parsec String () [GCode]
--pProgram = trace "pProgram" $ (pGCode `sepBy1` endOfLine) <* eof

pProgram = pGCode `sepBy` (many endOfLine) <* eof

parseGCode text = parse pProgram "(gcode)" text

