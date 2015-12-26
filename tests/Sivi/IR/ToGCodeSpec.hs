module Sivi.IR.ToGCodeSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi.IR.Base
import Sivi.IR.ToGCode
import Sivi.GCode
import Sivi.Operation.Types

spec :: SpecWith ()
spec = describe "toGCode" $ do
        it "transforms rapid moves into G00" $ 
                toGCode (IR [Move (V3 1 2 3) Rapid, Move (V3 4 5 6) Rapid]) `shouldBe` 
                        GCode [G00 (Just 1) (Just 2) (Just 3), G00 (Just 4) (Just 5) (Just 6)]
        it "transforms linear interpolations into G01" $ 
                toGCode (IR [Move (V3 7 8 9) (LinearInterpolation 100), Move (V3 10 11 12) (LinearInterpolation 200)]) `shouldBe`
                        GCode [G01 (Just 7) (Just 8) (Just 9) (Just 100), G01 (Just 10) (Just 11) (Just 12) (Just 200)]
        it "transforms clockwise arcs into G02" $
                toGCode (IR [Move (V3 4 2 3) Rapid, Move (V3 2 2 3) (Arc CW (V3 3 2 3) 100)]) `shouldBe`
                        GCode [G00 (Just 4) (Just 2) (Just 3), G02 (Just 2) (Just 2) (Just 3) (Just (-1)) Nothing Nothing (Just 100)]
        it "transforms clockwise arcs into G03" $
                toGCode (IR [Move (V3 4 2 3) Rapid, Move (V3 2 2 3) (Arc CCW (V3 3 2 3) 100)]) `shouldBe`
                        GCode [G00 (Just 4) (Just 2) (Just 3), G03 (Just 2) (Just 2) (Just 3) (Just (-1)) Nothing Nothing (Just 100)]
        it "transforms clockwise arcs into G03 (second example)" $
                toGCode (IR [Move (V3 5 2 3) Rapid, Move (V3 3 0 1) (Arc CCW (V3 4 1 2) 100)]) `shouldBe`
                        GCode [G00 (Just 5) (Just 2) (Just 3), G03 (Just 3) (Just 0) (Just 1) (Just (-1)) (Just (-1)) (Just (-1)) (Just 100)]
        it "transforms clockwise arcs into G03 (third example)" $
                toGCode (IR [Move (V3 (-2) 4 2) Rapid, Move (V3 (-1) 4 1) (Arc CCW (V3 (-2) 4 1) 200)]) `shouldBe`
                        GCode [G00 (Just (-2)) (Just 4) (Just 2), G03 (Just (-1)) (Just 4) (Just 1) Nothing Nothing (Just (-1)) (Just 200)]
        it "transforms probe moves into G38.2" $
                toGCode (IR [Move (V3 1 2 3) Rapid, Move (V3 4 5 6) (Probe 10)]) `shouldBe` 
                        GCode [G00 (Just 1) (Just 2) (Just 3), G38d2 (Just 4) (Just 5) (Just 6) (Just 10)]
        it "transforms comments into comments" $
                toGCode (IR [Comment "Comment"]) `shouldBe` GCode [GComment "Comment"]
        it "transforms pauses into M00" $
                toGCode (IR [Pause]) `shouldBe` GCode [M00]
        it "transforms DefCurPos into G92" $
                toGCode (IR [Move (V3 1 2 3) Rapid, DefCurPos (V3 11 12 13)]) `shouldBe`
                        GCode [G00 (Just 1) (Just 2) (Just 3), G92 (Just 11) (Just 12) (Just 13)]
