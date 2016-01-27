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
                        runOperation MF70 defaultCuttingParameters (zRepetition (-2.3) (Just 1) (const $ approach (V3 0 0 0) >> feed (V3 10 0 0))) `shouldBe`
                                IR [    Move (V3 0.0 0.0 50.0) Rapid,
                                        Move (V3 0.0 0.0 0.5) Rapid,
                                        Move (V3 0.0 0.0 (-0.5)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-0.5)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 10.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 0.0) Rapid,
                                        Move (V3 0.0 0.0 (-1.0)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-1.0)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 10.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 (-0.5)) Rapid,
                                        Move (V3 0.0 0.0 (-1.5)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-1.5)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 10.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 (-1.0)) Rapid,
                                        Move (V3 0.0 0.0 (-2.0)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-2.0)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 10.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 1.0) Rapid,
                                        Move (V3 0.0 0.0 (-1.2999999999999998)) Rapid,
                                        Move (V3 0.0 0.0 (-2.3)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-2.3)) LinearInterpolation {feedRate = 100.0}
                                ]

                it "repeats an operation with tool retraction" $
                        runOperation MF70 defaultCuttingParameters (zRepetition (-2.3) Nothing (const $ approach (V3 0 0 0) >> feed (V3 10 0 0))) `shouldBe`
                                IR [    Move (V3 0.0 0.0 50.0) Rapid,
                                        Move (V3 0.0 0.0 0.5) Rapid,
                                        Move (V3 0.0 0.0 (-0.5)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-0.5)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 0.0 0.0 (-0.5)) Rapid,
                                        Move (V3 0.0 0.0 (-1.0)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-1.0)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 0.0 0.0 (-1.0)) Rapid,
                                        Move (V3 0.0 0.0 (-1.5)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-1.5)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 0.0 0.0 (-1.5)) Rapid,
                                        Move (V3 0.0 0.0 (-2.0)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-2.0)) LinearInterpolation {feedRate = 100.0},
                                        Move (V3 0.0 0.0 (-2.0)) Rapid,
                                        Move (V3 0.0 0.0 (-2.3)) LinearInterpolation {feedRate = 30.0},
                                        Move (V3 10.0 0.0 (-2.3)) LinearInterpolation {feedRate = 100.0}
                                ]
        
