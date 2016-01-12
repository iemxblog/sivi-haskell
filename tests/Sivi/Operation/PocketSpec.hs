module Sivi.Operation.PocketSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = 
        describe "rectangularPocketZigzagP" $ do 
                it "makes a basic rectangular pocket pass" $ 
                        runOperation MF70 defaultCuttingParameters {initialPosition = V3 0 0 30} (rectangularPocketZigzagP 10 20 0.5 False) `shouldBe`
                                IR [
                                        Move (V3 1.5 1.5 30.0) Rapid
                                        , Move (V3 1.5 1.5 1.0) Rapid
                                        , Move (V3 1.5 1.5 0.0) (LinearInterpolation 30.0)
                                        , Move (V3 1.5 1.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 1.5 18.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 4.0 18.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 4.0 1.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 6.5 1.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 6.5 18.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 8.5 18.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 8.5 1.5 0.0) (LinearInterpolation 100.0)
                                        , Move (V3 1.5 1.5 0) Rapid
                                        , Move (V3 1.5 1.5 0) (LinearInterpolation 30.0)
                                        , Move (V3 8.5 1.5 0) (LinearInterpolation 100.0)
                                        , Move (V3 8.5 18.5 0) (LinearInterpolation 100.0)
                                        , Move (V3 1.5 18.5 0) (LinearInterpolation 100.0)
                                        , Move (V3 1.5 1.5 0) (LinearInterpolation 100.0)
                                ]               
                it "makes nothing for an a pocket with a dimension smaller than tool diameter" $ 
                        runOperation MF70 defaultCuttingParameters (rectangularPocketZigzagP 2 10 0.5 False) `shouldBe` IR []
