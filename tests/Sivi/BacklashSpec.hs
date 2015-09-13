module Sivi.BacklashSpec (
	spec	
) where

import Test.Hspec
import Linear
import Sivi.Backlash
import Sivi.IR

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

expectedPos :: [V3 Double]
expectedPos = 
	[ V3 1 1 0.5
	, V3 2 2 (-0.5) 
	, V3 3 3 (-0.5)
	, V3 5 4 (-0.5) 
	, V3 5 3.5 (-0.5) 
	, V3 7 2.5 (-0.5) 
	, V3 8 1.5 (-0.5) 
	, V3 8 2 (-0.5) 
	, V3 12 3 (-0.5) 
	, V3 11.5 2.5 (-0.5)
	, V3 10.5 (-0.5) (-0.5) 
	, V3 9.5 (-1.5) (-0.5)
	, V3 6.5 (-1.5) (-0.5)
	, V3 6.5 (-1) (-0.5)
	, V3 5.5 0 (-0.5)
	, V3 4.5 0 (-0.5)
	, V3 3.5 0 (-0.5)
	, V3 2.5 0 (-0.5)
	, V3 2.5 (-0.5) (-0.5)
	, V3 1.5 (-1.5) (-0.5)
	]
initPos :: [V3 Double]
initPos = [V3 0 0 0, V3 1 1 1]

backlash :: V3 Double
backlash = V3 0.5 0.5 0.5

spec = describe "backlashCompensation" $ do
		let fRapid = map (\x -> Move x Rapid)
		let fLinearInterpolation = map (\x -> Move x (LinearInterpolation 100))
		it "compensates backlash for rapid moves" $ 
			backlashCompensation (fRapid pos) initPos backlash  `shouldBe` fRapid (initPos ++ expectedPos)

		it "compensates backlash for linear interpolations" $ 
			backlashCompensation (fLinearInterpolation pos) initPos backlash  `shouldBe` (fRapid initPos ++ fLinearInterpolation expectedPos)
