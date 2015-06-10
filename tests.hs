import Test.HUnit
import Linear

import IR

initBacklash = [V3 0 0 0, V3 1 1 1]

p1 = V3 10 5 1
p2 = V3 9 6 2
p3 = V3 12 3 0

backlashX = 0.04
backlashY = 0.06
backlashZ = 0.07

vBacklashX = V3 backlashX 0 0
vBacklashY = V3 0 backlashY 0
vBacklashZ = V3 0 0 backlashZ


points :: [V3 Double]
points = [p1, p2, p3]

pointsWithCompensation :: [V3 Double]
pointsWithCompensation = [p1, p1 - vBacklashX, p2 - vBacklashX, p2 - vBacklashY - vBacklashZ, p3 - vBacklashY - vBacklashZ]


prog1 :: Program
prog1 = map Rapid points

prog1WithCompensation :: Program
prog1WithCompensation = map Rapid $ initBacklash ++ pointsWithCompensation


prog2 :: Program
prog2 = map (\p -> LinearInterpolation p 100) points


prog2WithCompensation :: Program
prog2WithCompensation = map (\p -> LinearInterpolation p 100) pointsWithCompensation

testRapid = TestCase (assertEqual (backlashCompensation prog1 initBacklash (backlashX, backlashY, backlashZ)) prog1withCompensation)
