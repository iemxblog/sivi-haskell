module Sivi.Operation.BoundingBoxSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = describe "BoundingBox module" $ do
        describe "boundingBox" $ do
                let boundingBox = runOperation MF70 defaultCuttingParameters
                it "calculates the bounding box of a circular pocket" $ 
                        boundingBox (circularPocket 20 5 0.5) `shouldBe` BoundingBox [Boundary (-10, 10), Boundary (-10, 10), Boundary (-5, 0)]
                it "calculates the bounding box of a translated circular pocket" $ 
                        boundingBox (translate (V3 15 20 (-5)) (circularPocket 20 5 0.5)) `shouldBe` BoundingBox [Boundary (5, 25), Boundary (10, 30), Boundary (-10, -5)]
                it "calculates the bounding box of a square" $ 
                        boundingBox (square 30) `shouldBe` BoundingBox [Boundary (-1.5, 31.5), Boundary (-1.5, 31.5), Boundary (0, 0.5)]
                it "calculates the bounding box of two circularPockets with a big tool retraction" $
                        boundingBox (chain 100 [circularPocket 20 5 0.5, translate (V3 25 0 0) (circularPocket 20 5 0.5)]) `shouldBe`
                                BoundingBox [Boundary (-10, 35), Boundary (-10, 10), Boundary (-5, 0)]
                it "calculates the bounding box of a simple line (takes into account the *beginning* and end of line)" $
                    boundingBox (rapid (V3 10 10 0) >> feed (V3 20 10 0)) `shouldBe`
                        BoundingBox [Boundary (8.5, 21.5), Boundary (8.5, 11.5), Boundary (0, 0.5)] 


        describe "|>|" $ do
                it "places a square next to another square" $
                        (runOperation MF70 defaultCuttingParameters $ square 10 |>| square 15 :: IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ square 10 +^+ translate (V3 (10+3) 0 0) (square 15))
                        -- +3 because of the diameter of the tool
                it "translation of 2 squares placed next to each other" $
                        ((runOperation MF70 defaultCuttingParameters (translate (V3 10 0 (-10) ) (square 10 |>| square 15))) :: IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ translate (V3 10 0 (-10)) (square 10 +^+ translate (V3 (10+3) 0 0) (square 15)))
                        
                it "arc placed next to a diagonal line" $ 
                        (runOperation MF70 defaultCuttingParameters (feed (V3 10 10 0) |>| (approach (V3 0 0 0) >> arc CCW (V3 0 5 0) (V3 0 10 0))) :: IR) `shouldBe`
                                IR [Move (V3 10.0 10.0 0.0) (LinearInterpolation 100.0)
                                , Move (V3 10.0 10.0 1.0) Rapid
                                , Move (V3 13.0 0.0 1.0) Rapid
                                , Move (V3 13.0 0.0 0.0) (LinearInterpolation 30.0)
                                , Move (V3 13.0 10.0 0.0) (Arc {direction = CCW, center = V3 13.0 5.0 0.0, Sivi.IR.feedRate = 100.0})
                                ]


