module Sivi.Operation.RepetitionSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi hiding (feedRate)
import Sivi.IR


spec :: SpecWith ()
spec = 
        describe "zRepetition" $ do 

                it "repeats an operation with tool retraction" $
                        (runOperation MF70 defaultCuttingParameters (zRepetition (-2.3) (Just 1) (const $ approach (V3 0 0 0) >> feed (V3 10 0 0)))::IR) `shouldBe`
                                (runOperation MF70 defaultCuttingParameters $ do
                                    let op = approach (V3 0 0 0) >> feed (V3 10 0 0)
                                    translate (V3 0 0 (-0.5)) op
                                    retract 1
                                    translate (V3 0 0 (-1)) op
                                    retract 1
                                    translate (V3 0 0 (-1.5)) op
                                    retract 1
                                    translate (V3 0 0 (-2)) op
                                    retract 1
                                    translate (V3 0 0 (-2.3)) op)

                it "repeats an operation without tool retraction" $
                        (runOperation MF70 defaultCuttingParameters (zRepetition (-2.3) Nothing (const $ approach (V3 0 0 0) >> feed (V3 10 0 0)))::IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ do
                                let op = approach (V3 0 0 0) >> feed (V3 10 0 0)
                                translate (V3 0 0 (-0.5)) op
                                translate (V3 0 0 (-1)) op
                                translate (V3 0 0 (-1.5)) op
                                translate (V3 0 0 (-2)) op
                                translate (V3 0 0 (-2.3)) op)
