{-# LANGUAGE OverloadedStrings #-}
module Sivi.IR.FromGCodeSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi.IR.Base
import Sivi.GCode.Base
import Sivi.GCode.Parser
import Sivi.IR.FromGCode

spec = describe "fromGCode" $ do
	it "transforms a basic program into IR" $ do
		let prg = "G00 X1 Z2\nG01 Y2 F100\nX3\nY4\nX2 Y2 Z0\nG02 X4 I1 J-1\nG38.2 X-10 F10\nY-10\nG92 X0 Y0\nZ-10"
		case parse prg of
			Left err -> expectationFailure $ "Parse error : " ++ err
			Right gcode -> fromGCode gcode `shouldBe` 
				[ Move (V3 1 0 2) Rapid
				, Move (V3 1 2 2) (LinearInterpolation 100)
				, Move (V3 3 2 2) (LinearInterpolation 100)
				, Move (V3 3 4 2) (LinearInterpolation 100)
				, Move (V3 2 2 0) (LinearInterpolation 100)
				, Move (V3 4 2 0) (Arc {direction = CW, center = V3 3 1 0, feedRate = 100})
				, Move (V3 (-10) 2 0) (Probe {feedRate = 10})
				, Move (V3 (-10) (-10) 0) (Probe {feedRate = 10})
				, DefCurPos (V3 0 0 0)
				, Move (V3 0 0 (-10)) (Probe {feedRate = 10})
				]
