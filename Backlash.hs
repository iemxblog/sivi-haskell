import Linear
import Data.List
import IR

pos :: [V3 Double]
pos = [   V3 2 2 0
	, V3 3 3 0
	, V3 5 4 0
	, V3 7 3 0
	, V3 8 2 0 
	, V3 12 3 0 
	, V3 11 0 0
	, V3 10 (-1) 0
	, V3 7 (-1) 0
	, V3 6 0 0
	, V3 5 0 0
	, V3 4 0 0
	, V3 3 0 0
	, V3 2 (-1) 0
	]

initPos :: [V3 Double]
initPos = [V3 0 0 0, V3 1 1 1]

backlash :: V3 Double
backlash = V3 0.5 0.5 0.5

diff :: Num a => [a] -> [a]
diff xs = [b-a | (a,b) <- zip xs (tail xs)]


mem :: (Eq a, Num a) => a -> a -> a
mem a 0 = a
mem 0 b = b
mem a b = b

dirc :: (Eq a, Num a) => a -> a -> a
dirc (-1) 1 = 1
dirc 1 (-1) = -1
dirc _ _ = 0

onV3 :: (a -> a -> a) -> V3 a -> V3 a -> V3 a
onV3 f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')

--dir xs = scanl (onV3 mem) (V3 0 0 0) (map signum $ diff xs)
--
--dirChange xs = zipWith (onV3 dirc) (dir xs) (tail $ dir xs)
--
--compensation xs = map (backlash*) $ scanl (+) (V3 0 0 0) (dirChange xs)
--
--compensatedPos xs = zipWith (+) xs (compensation xs)
--
--extraMove xs = [if a == V3 0 0 0 then V3 0 0 0 else b + c  | (a, b, c) <- zip3 (dirChange xs) xs (tail $ compensation xs)]
--
--
--cfusion :: [a] -> [a] -> (a -> Bool) -> [a]
--cfusion [] ys _ = ys
--cfusion xs [] _ = xs
--cfusion (x:xs) (y:ys) p
--	| p y  = x : y : cfusion xs ys p
--	| otherwise = x : cfusion xs ys p
--
--backlashCompensation xs = cfusion (compensatedPos xs)  (extraMove xs) (/= V3 0 0 0)

backlash2 :: Program -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> Program
backlash2 [] _ _ _ _ = []
backlash2 ((Move pos mp):xs) backlash ppos pdir pcomp = 
	if extraMove == V3 0 0 0 then (Move compensatedPos mp) : backlash2 xs backlash pos newdir compensation
	else (Move extraMove mp) : (Move compensatedPos mp) : backlash2 xs backlash pos newdir compensation
	where 
		sgn = signum (pos-ppos)
		newdir = onV3 mem pdir sgn
		dirChange = onV3 dirc pdir newdir
		compensation = pcomp + backlash * dirChange
		compensatedPos = pos + compensation
		extraMove = if dirChange == V3 0 0 0 then V3 0 0 0 else ppos + compensation

prog = map (\x -> Move x Rapid) (initPos ++ pos)
