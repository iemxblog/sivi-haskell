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
import Sivi.Misc.SharedParsers
import Control.Applicative((<*), (*>))

-- | Parses a GCode word.
word :: Char 					-- ^ Name of the word (X or Y or Z, ...)
	-> Parsec String () (Maybe Double)	-- ^ Result is Nothing if word is not mentioned, or (Just value)
word wn = (optionMaybe $ do
		char wn
		double
	   ) <?> "GCode word"


-- | Parses a list of GCode words. Fails if all words are absent.
pParams :: [Char]				-- ^ The names of the words
	-> Parsec String () [Maybe Double]	-- ^ The list of word values
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

-- | Parses a G92
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

-- | Parses a line
pProgramLine :: Parsec String () GCode
pProgramLine = pGCode <* (many1 endOfLine)

-- | Parses a GCode program (list of commands)
pProgram = many pProgramLine <* eof

-- | Parses a GCode program.
parseGCode :: String				-- ^ The GCode program
		-> Either String [GCode]	-- ^ The resulting GCode data structure (or parse error)
parseGCode text = case parse pProgram "(gcode)" text of
			Left pe -> Left (show pe)
			Right gcode -> Right gcode 

