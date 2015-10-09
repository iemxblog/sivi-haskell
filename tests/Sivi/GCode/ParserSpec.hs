module Sivi.GCode.ParserSpec (
	spec
) where

import Test.Hspec
import Sivi.GCode

spec :: SpecWith ()
spec = describe "fromGCode" $ do
	it "transforms a basic program into IR" $ do
		let prg = "G00 X1 Z2\nG01 Y2 F100\nX3\nY4\nX2 Y2 Z0\nG02 X4 I1 J-1\nG38.2 X-10 F10\nY-10\nG92 X0 Y0\nZ-10"
		case parseGCode prg of
			Left err -> expectationFailure $ "Parse error : " ++ show err
			Right gcode -> gcode `shouldBe` 
				[ g00 {x = Just 1,  z = Just 2}
				, g01 {y = Just 2, f = Just 100}
				, cline {x = Just 3}
				, cline {y = Just 4}
				, cline {x = Just 2, y = Just 2, z = Just 0}
				, g02 {x = Just 4, i = Just 1, j = Just (-1)}
				, g38d2 {x = Just (-10), f = Just 10}
				, cline {y = Just (-10)}
				, g92 {x = Just 0, y = Just 0}
				, cline {z = Just (-10)}	
				]
	it "fails when no parameters are provided for a G00" $ 
		case parseGCode "G00\nG01 X10" of
			Left pe -> show pe `shouldBe` "\"(gcode)\" (line 1, column 4):\nunexpected \"\\n\"\nexpecting GCode word"
			Right _ -> expectationFailure $ "Parsing should have failed."

