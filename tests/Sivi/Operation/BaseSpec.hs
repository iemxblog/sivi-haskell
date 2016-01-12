module Sivi.Operation.BaseSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = 
        describe "zigzag" $ do 
                it "makes a basic zigzag operation" $ do
                        let path = [
                                [V3 0 0 0, V3 10 0 0]
                                , [V3 0 5 0, V3 10 5 0]
                                , [V3 0 10 0, V3 10 10 0]
                                , [V3 0 15 0, V3 10 15 0]
                                , [V3 0 20 0, V3 10 20 0]
                                ]
                        runOperation MF70 defaultCuttingParameters {initialPosition = V3 0 0 30} (zigzag path) `shouldBe`
                                IR [
                                        Move (V3 0 0 30) Rapid
                                        , Move (V3 0 0 1) Rapid
                                        , Move (V3 0 0 0) (LinearInterpolation 30)
                                        , Move (V3 0 0 0) (LinearInterpolation 100)
                                        , Move (V3 10 0 0) (LinearInterpolation 100)
                                        , Move (V3 10 5 0) (LinearInterpolation 100)
                                        , Move (V3 0 5 0) (LinearInterpolation 100)
                                        , Move (V3 0 10 0) (LinearInterpolation 100)
                                        , Move (V3 10 10 0) (LinearInterpolation 100)
                                        , Move (V3 10 15 0) (LinearInterpolation 100)
                                        , Move (V3 0 15 0) (LinearInterpolation 100)
                                        , Move (V3 0 20 0) (LinearInterpolation 100)
                                        , Move (V3 10 20 0) (LinearInterpolation 100)
                                ]
                it "makes nothing for an empty list" $ do
                        let path = []
                        runOperation MF70 defaultCuttingParameters (zigzag path) `shouldBe` IR []
                it "makes nothing for a list of empty lists" $ do
                        let path = [[], [], [], []]
                        runOperation MF70 defaultCuttingParameters (zigzag path) `shouldBe` IR []
