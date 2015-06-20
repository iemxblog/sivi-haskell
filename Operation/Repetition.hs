module Operation.Repetition (
	circularRepetition
	, gridRepetition
) where

import Operation.Base
import IR
import Linear

-- | Chains two operations, and adds a tool rectraction before each operation
next :: Double 			-- ^ z_safe : Tool retraction 
	-> Operation IR		-- ^ op1 : First operation
	 -> Operation IR	-- ^ op2 : Second operation
	 -> Operation IR	-- Rectraction + Operation 1 + Retraction + Operation 2
next z_safe o1 o2 = retract z_safe +++ o1 +++ retract z_safe +++ o2 

repetition :: [V3 Double] -> Double -> Operation IR -> Operation IR
repetition xs z_safe op = foldr (+++) noOp $ map (\v -> retract z_safe +++ translate v op) xs

-- | Repeats an operation on a circle (and adds a tool retraction each time)
circularRepetition :: Double			-- ^ d : Diameter 
			-> Int			-- ^ n : Number of repetitions
			-> Double		-- ^ start_angle : Start angle
			-> Double		-- ^ z_safe : Tool retraction 
			-> Operation IR		-- ^ op : Operation to repeat
			-> Operation IR		-- ^ Retraction + Operation 1 on circle + Retraction + Operation 2 on circle + ...
circularRepetition d n start_angle z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 (d/2*cos(f i)) (d/2*sin(f i)) 0 | i <- [0..n-1]]
		f a = fromIntegral(a)*2*pi/(fromIntegral n)+start_angle*pi/180

-- | Repeats an operation on a grid (and adds a tool retraction each time)
gridRepetition :: Int			-- ^ nx : Number of repetitions on X axis
		 -> Int			-- ^ ny : Number of repetitions on Y axis
		 -> Double		-- ^ space_x : Space between each operation on X axis
		 -> Double		-- ^ space_y : Space between each operation on Y axis
		 -> Double		-- ^ z_safe : Tool retraction
		 -> Operation IR	-- ^ op : Operation to repeat
		 -> Operation IR	-- ^ Retraction + Operation 1 on grid + Retraction + Operation 2 on grid + ...
gridRepetition nx ny space_x space_y z_safe op = repetition pos_list z_safe op
	where
		pos_list = [V3 ((fromIntegral x)*space_x) ((fromIntegral y)*space_y) 0 | x <- [1..nx-1], y <- [1..ny-1]]
