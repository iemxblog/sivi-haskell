module Operation.Pocket (
	circularPocket
	, rectangularPocket
)
where

import Operation.Base
import Operation.BasicShape
import IR
import Linear

-- | Generates an archimedean spiral. (used in circularPocket)
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
				c <- circleFromHere -- the spiral ends at radius = d-df/2, so we start a circle from here
				return (a ++ sp ++ c)
				


rectangularSpiral' :: (Num a, Integral b) => (a, a)
			-> b
			-> a
			-> a 
			-> [(a, a)]
rectangularSpiral' (xo, yo) i sx sy  = 
		[(xo, yo), (xo+i'*sx, yo), (xo+i'*sx, yo+i'*sy), (xo-sx, yo+i'*sy)] ++ rectangularSpiral' (xo-sx, yo-sy) (i+2) sx sy
		where
			i' = fromIntegral i

rectangularSpiral :: Num a => 
			a
			-> a
			-> [(a, a)]
rectangularSpiral = rectangularSpiral' (0, 0) 1

rectangularPocket :: 	Double 
			-> Double 
			-> Double 
			-> Operation IR
rectangularPocket lx ly step_over = do
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
			return (a ++ sp ++ r)
						
