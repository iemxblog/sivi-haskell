{-|
Module		: Sivi.Misc.SharedParsers
Description	: Custom parsers used in several modules
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Misc.SharedParsers
(
	double
	, lexeme
	, symbol
	, condition
) where

import Text.Parsec
import Data.Char
import Control.Applicative((<*), (*>))

-- | Parses a double.
double :: Parsec String () Double
double = do
		sign <- option "" (string "-")
		integerPart <- many1 (satisfy isDigit)
		decimalPart <- option "" $ do
			p <- char '.' 
			d <- many1 (satisfy isDigit)
			return (p:d)
		return (read (sign++integerPart++decimalPart) :: Double)

-- | Custom version of 'Text.Parsec.Token.lexeme'. (This custom version doesn't parse newlines).
lexeme :: Parsec String () a -> Parsec String () a
lexeme p = many (satisfy (==' ')) *> p <* many (satisfy (==' '))

-- | Custom version of 'Text.Parsec.Token.symbol'. (This custom version doesn't parse newlines).
symbol :: String -> Parsec String () String
symbol xs = lexeme (string xs)

-- | Runs a parser, and checks its result with the supplied function. If result is False, the parser fails.
condition :: (a -> Bool) 		-- ^ Function used to check the parser result
		-> Parsec String () a	-- ^ Parser of which we want to check the result
		-> Parsec String () a
condition f p  = do
	r <- p
	case f r of
		True -> return r
		False -> parserFail ""

