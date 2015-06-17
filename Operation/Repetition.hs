module Operation.Repetition (
	circularRepetition
	, gridRepetition
) where

import Operation.Base
import Linear

next :: Double -> Operation -> Operation -> Operation
next z_safe o1 o2 = retract z_safe +++ o1 +++ retract z_safe +++ o2 


repetition :: [V3 Double] -> Double -> Operation -> Operation
repetition xs z_safe op = foldr (+++) noOp $ map (\v -> retract z_safe +++ translate v op) xs

circularRepetition :: Double -> Int -> Double -> Double -> Operation -> Operation
circularRepetition d n start_angle z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 (d/2*cos(f i)) (d/2*sin(f i)) 0 | i <- [0..n-1]]
		f a = fromIntegral(a)*2*pi/(fromIntegral n)+start_angle*pi/180

gridRepetition :: Int -> Int -> Double -> Double -> Double -> Operation -> Operation
gridRepetition nx ny space_x space_y z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 ((fromIntegral x)*space_x) ((fromIntegral y)*space_y) 0 | x <- [1..nx-1], y <- [1..ny-1]]
