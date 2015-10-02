{-|
Module		: Sivi.IR.Base
Description	: IR Datatype declaration
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.IR.Base
(
	Tool(..)
	, ArcDirection(..)
	, MoveParams(..)
	, Instruction(..)
	, IR
	, Tree(..)
	, IRTree
	, flatten
) where

import Linear

-- | Tool data type.
-- Used for tool changes, radius compensation.
data Tool = 
	EndMill { diameter :: Double, len :: Double }
	| BallEndMill { diameter :: Double, shankDiameter :: Double, len :: Double } 
	| ProbeTool { diameter :: Double, len :: Double }
	deriving (Eq, Show)

type Coordinate = Double
type FeedRate = Double

data ArcDirection = 	CW 	-- ^ Clockwise
			| CCW 	-- ^ Counterclockwise
			deriving (Eq, Show)

-- | Parameters for the 'Move' data constructor.
-- A move is either a Rapid move, or a linear interpolation with a feed rate, or an arc. So there is only one data constructor for moves.
data MoveParams = Rapid 
		| LinearInterpolation { feedRate :: FeedRate } 
		| Arc { direction :: ArcDirection, center :: V3 Double, feedRate :: FeedRate }
		| Probe {feedRate :: FeedRate }
		deriving (Eq, Show)

-- | Intermediate Representation
data Instruction = 
	Move (V3 Coordinate) MoveParams		-- ^ Rapid, Linear interpolation, Arc, ... (all actions that make the tool move)
	| Comment String			-- ^ Comments
	| Pause					-- ^ Pause (waits for user interaction, translated to a M00 GCode). In GRBL, program will stop until Cycle Start is pressed.
	| DefCurPos (V3 Coordinate)		-- ^ Define current position
	deriving (Eq, Show)

type IR = [Instruction]

data Tree v a = 
	Leaf a
	| Node v [Tree v a]

-- | Tree of instructions
type IRTree = Tree String Instruction

-- | Transforms a tree of intructions to a list of instructions. Annotations are transformed into comments.
-- It could be possible to make a more general function which accepts more types, but would probably be overkill ??
flatten :: IRTree-> [Instruction]
flatten (Leaf i) = [i]
flatten (Node v ts) = opening ++ concatMap flatten ts ++ closing
	where
		opening = if v == "" then [] else [Comment ("> " ++ v)] 
		closing = if v == "" then [] else [Comment ("< " ++ v)]
