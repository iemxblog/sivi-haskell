module IR 
(
	Tool(..)
	, MoveParams(..)
	, IR(..)
	, Program
) where

import Linear

data Tool = 
	EndMill { name :: String, diameter :: Double, length :: Double }
	| BallEndMill { name :: String, diameter :: Double, shankDiameter :: Double, length :: Double } 
	deriving (Eq, Show)

type Coordinate = Double
type FeedRate = Double

data MoveParams = Rapid | LinearInterpolation { feedRate :: FeedRate } deriving (Eq, Show)

data IR = 
	Move (V3 Coordinate) MoveParams
	| ChangeTool Tool
	| Comment String
	| Pause
	deriving (Eq, Show)

type Program = [IR]


example :: Program
example = [
	ChangeTool (EndMill "01" 3 42)
	, Move (V3 1 0 0) Rapid
	, Move (V3 2 2 0) (LinearInterpolation 100)
	, Pause
	, Move (V3 0 0 0) Rapid,
	Comment "Commentaire"
	]


compileIR :: IR -> String
compileIR (Move (V3 x y z) Rapid) = "G00 X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z
compileIR (Move (V3 x y z) (LinearInterpolation f)) = "G01 X" ++ show x ++ " Y" ++ show y ++ " Z" ++ show z ++ " F" ++ show f
compileIR (ChangeTool t) = "M6 T" ++ name t
compileIR (Comment s) = "(" ++ s ++ ")"
compileIR Pause = "M00"

compile :: Program -> String
compile p = unlines $ map compileIR p
