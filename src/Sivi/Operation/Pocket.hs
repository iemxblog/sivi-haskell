{-|
Module		: Sivi.Operation.pocket
Description	: Pocketing operations (circular pocket, rectangular pocket, ...)
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.Pocket (
	circularPocket
	, rectangularPocket
)
where

import Sivi.Operation.Base
import Sivi.Operation.BasicShape
import Sivi.Operation.Repetition
import Sivi.IR
import Linear

-- | Generates an archimedean spiral. (used in 'circularPocket')
-- Increment in angle is +1 degree. 
archimedeanSpiral :: 	Double			-- ^ d : External diameter of the spiral
			-> Double		-- ^ step_over : the end mill covers step_over mm of the precedent turn
			-> Operation IRTree	-- ^ Resulting operation
archimedeanSpiral d step_over = 
	do
		a <- approach (V3 0 0 0) 
		df <- getToolDiameter
		let angle = [i * pi / 180 | i <- [0..]] -- angle = [1 degree, 2 degrees, ...]
		let radius = takeWhile (\r -> r+df/2 < d/2) [a * (df - step_over) / (2 * pi) | a <- angle]
		let lastPoint = ((d-df)/2, pi*(d-df)/(df-step_over))  -- So at the end we are at the exact radius (d-df)/2
		sp <- opsequence [feed (V3 (r*cos a) (r*sin a) 0) | (r, a) <- zip radius angle ++ [lastPoint]]
		return $ Node "" [a, sp]

-- | Generates a single pass of a circular pocket.
circularPocketP :: Double			-- ^ d : Diameter of the pocket
		-> Double			-- ^ step_over : The end mill covers step_over mm of the precedent turn (in the spiral)
		-> Operation IRTree		-- ^ Resulting operation
circularPocketP d step_over = do
				a <- approach (V3 0 0 0)
				sp <- archimedeanSpiral d step_over
				c <- circleFromHere -- the spiral ends at radius = d-df/2, so we start a circle from here
				return $ Node "" [a, sp, c]

-- | Generates a circular pocket.
circularPocket :: Double			-- ^ d : Diameter of the pocket
		-> Double			-- ^ depth : Depth of the pocket
		-> Double			-- ^ step_over : The end mill covers step_over mm of the precedent turn (in the spiral)
		-> Operation IRTree		-- ^ Resulting operation
circularPocket d depth step_over = zRepetition depth Nothing (circularPocketP d step_over)
				

-- | Generates the coordinates of a rectangular spiral (in 2D).
-- Recursive function which generates a piece of spiral, then calls itself to generate a bigger piece.
rectangularSpiralR :: (Num a, Integral b) => (a, a)	-- ^ (xo, yo) : Coordinates of the origin of the spiral
			-> b				-- ^ i : Multiplicator coefficient to generate bigger and bigger pieces 
			-> a				-- ^ sx : Spacing between each turn for the x axis
			-> a 				-- ^ sy : Spacing between each turn for the y axis
			-> [(a, a)]			-- ^ Resulting rectangular spiral
rectangularSpiralR (xo, yo) i sx sy  = 
		[(xo, yo), (xo+i'*sx, yo), (xo+i'*sx, yo+i'*sy), (xo-sx, yo+i'*sy)] ++ rectangularSpiralR (xo-sx, yo-sy) (i+2) sx sy
		where
			i' = fromIntegral i

-- | Interface for 'rectangularSpiralR'
rectangularSpiral :: Num a => 
			a				-- ^ sx : Spacing betwenn each turn for the x axis
			-> a				-- ^ sy : Spacing between each turn for the y axis
			-> [(a, a)]			-- ^ Resulting rectangular spiral
rectangularSpiral = rectangularSpiralR (0, 0) 1

-- | Generates a single pass of a rectangular pocket. The P means pass.
rectangularPocketP :: 	Double 				-- ^ lx : Size of the pocket on the x axis
			-> Double 			-- ^ ly : Size of the pocket on the y axis
			-> Double 			-- ^ step_over : The end mill covers step_over mm of the precedent turn
			-> Operation IRTree		-- Resulting operation
rectangularPocketP lx ly step_over = do
			a <- approach (V3 0 0 0)
			df <- getToolDiameter		
			let initial_spacing = df - step_over
			let cycles_x = floor $ (lx - df)/(2 * initial_spacing)
			let cycles_y = floor $ (ly - df)/(2 * initial_spacing)
			let cycles = fromIntegral $ max cycles_x cycles_y
			let spacing_x = (lx - df)/(2 * cycles)
			let spacing_y = (ly - df)/(2 * cycles)
			let sp' = takeWhile (\(x,y) -> abs x <= lx/2 && abs y <= ly/2) $ rectangularSpiral spacing_x spacing_y
			sp <- opsequence [feed (V3 x y 0) | (x, y) <- sp']
			r <- centeredRectangle (lx-df) (ly-df)
			return $ Node "" [a, sp, r]

-- | Generates a rectangular pocket.
rectangularPocket :: 	Double				-- ^ lx : Size of the pocket on the x axis
			-> Double			-- ^ ly : Size of the pocket on the y axis
			-> Double			-- ^ depth : Depth of the pocket
			-> Double			-- ^ step_over : Then end mill covers step_over mm of the precedent turn
			-> Operation IRTree		-- ^ Resulting operation
rectangularPocket lx ly depth step_over = zRepetition depth Nothing (rectangularPocketP lx ly step_over)
