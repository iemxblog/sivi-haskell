module Operation.Pocket (
	archimedeanSpiral
	, circularPocket
)
where

import Operation.Base
import Operation.BasicShape
import IR
import Linear

-- | Generates an archimedean spiral.
-- Increment in angle is +1 degree. 
archimedeanSpiral :: 	Double
			-> Double
			-> Operation IR
archimedeanSpiral d step_over = 
	do
		a <- approach (V3 0 0 0) 
		df <- getToolDiameter
		let angle = [i * pi / 180 | i <- [0..]] -- angle = [1 degree, 2 degrees, ...]
		let radius = takeWhile (\r -> r+df/2 < d/2) [a * (df - step_over) / (2 * pi) | a <- angle]
		let lastPoint = ((d-df)/2, pi*(d-df)/(df-step_over))  -- So at the end we are at the exact radius (d-df)/2
		sp <- opsequence [feed (V3 (r*cos(a)) (r*sin(a)) 0) | (r, a) <- zip radius angle ++ [lastPoint]]
		return (a ++ sp)

circularPocket :: Double
		-> Double
		-> Operation IR
circularPocket d step_over = do
				a <- approach (V3 0 0 0)
				df <- getToolDiameter
				sp <- archimedeanSpiral d step_over
				--c <- circle (d-df)
				c <- circleFromHere -- #########################################################################
				return (a ++ sp ++ c)
				
