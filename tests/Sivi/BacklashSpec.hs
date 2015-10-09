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

expectedPos1 :: [V3 Double]
expectedPos1 = 
	[ 
		V3 0 0 0
		, V3 0.2 0.3 0.4
		, V3 1.2 1.3 1.4
	]

expectedPos2 :: [V3 Double]
expectedPos2 = 
	[
		V3 1.2 1.3 1
		, V3 2.2 2.3 0
		, V3 3.2 3.3 0
		, V3 5.2 4.3 0
		, V3 5.2 4 0
		, V3 7.2 3 0
		, V3 8.2 2 0
		, V3 8.2 2.3 0
		, V3 12.2 3.3 0
		, V3 12 3 0
		, V3 11 0 0
		, V3 10 (-1) 0	
		, V3 7 (-1) 0
		, V3 7 (-0.7) 0
		, V3 6 0.3 0
		, V3 5 0.3 0
		, V3 4 0.3 0
		, V3 3 0.3 0
		, V3 3 0 0
		, V3 2 (-1) 0
	]

initPos :: V3 Double
initPos = V3 0 0 0

backlashValues :: V3 Double
backlashValues = V3 0.2 0.3 0.4

withProbe :: IR
withProbe = [
	Move (V3 5 4 0) Rapid
	, Move (V3 5 3 0) Rapid
	, Move (V3 6 6 0) Rapid
	, Move (V3 4 6 0) (Probe 10)
	, DefCurPos (V3 0 0 0)
	, Move (V3 1 1 0) Rapid
	, Move (V3 3 1 0) (Probe 10)
	, DefCurPos (V3 0 0 0)
	, Move (V3 (-2) 2 0) Rapid
	]

expectedWithProbe :: IR
expectedWithProbe = [
	Move (V3 0 0 0) Rapid
	, Move (V3 0.2 0.3 0.4) Rapid
	, Move (V3 1.2 1.3 1.4) Rapid
	, Move (V3 1.2 1.3 1) Rapid
	, Move (V3 5.2 4.3 0) Rapid
	, Move (V3 5.2 4 0) Rapid
	, Move (V3 5.2 3 0) Rapid
	, Move (V3 5.2 3.3 0) Rapid
	, Move (V3 6.2 6.3 0) Rapid
	, Move (V3 6 6.3 0) (LinearInterpolation 10)
	, Move (V3 4 6.3 0) (Probe 10)
	, DefCurPos (V3 0 0.3 0)
	, Move (V3 0.2 0.3 0) Rapid
	, Move (V3 1.2 1.3 0) Rapid
	, Move (V3 3.2 1.3 0) (Probe 10)
	, DefCurPos (V3 0.2 0.3 0)
	, Move (V3 0 0.3 0) Rapid
	, Move (V3 (-2) 2.3 0) Rapid
	]
	
spec :: SpecWith ()
spec = describe "backlashCompensation" $ do
		let fRapid = map (\x -> Move x Rapid)
		let fLinearInterpolation = map (\x -> Move x (LinearInterpolation 100))
		it "compensates backlash for rapid moves" $ 
			backlash initPos backlashValues (fRapid pos)  `shouldBe` fRapid (expectedPos1 ++ expectedPos2)

		it "compensates backlash for linear interpolations" $ 
			backlash initPos backlashValues (fLinearInterpolation pos)  `shouldBe` fRapid expectedPos1 ++ fLinearInterpolation expectedPos2

		it "compensates backlash for probing motions (G38.2) and offsets (G92)" $
			backlash initPos backlashValues withProbe `shouldBe` expectedWithProbe
