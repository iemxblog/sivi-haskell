import Test.HUnit
import Linear
import IR
import Backlash

initPos = [V3 0 0 0, V3 1 1 1]

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


prog1 :: IR
prog1 = map (\p -> Move p Rapid) points

prog1WithCompensation :: IR
prog1WithCompensation = map (\p -> Move p Rapid) $ initPos ++ pointsWithCompensation


prog2 :: IR
prog2 = map (\p -> Move p (LinearInterpolation 100)) points


prog2WithCompensation :: IR
prog2WithCompensation = map (\p -> Move p Rapid) initPos ++ map (\p -> Move p (LinearInterpolation 100)) pointsWithCompensation

testRapid = TestCase (assertEqual "for backlashCompensation with Rapid move, " (backlashCompensation prog1 initPos (vBacklashX + vBacklashY + vBacklashZ)) prog1WithCompensation)
testLinearInterpolation = TestCase (assertEqual "for backlashCompensation with LinearInterpolation move, " (backlashCompensation prog2 initPos (vBacklashX + vBacklashY + vBacklashZ)) prog2WithCompensation)

tests = TestList [ TestLabel "testRapid" testRapid, TestLabel "testLinearInterpolation" testLinearInterpolation ]
