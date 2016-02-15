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
                        BoundingBox [Boundary (8.5, 21.5), Boundary (8.5, 11.5), Boundary (0, 0)] 

                it "always returns the same result (equal to depth of cut) for an approach move, independently of the initial position of the tool" $ do
                    let zs = [10, 5, 0, -3, -3.1, -3.5, -3.6, -3.9, -4, -4.5, -10]
                    let bz (BoundingBox [_, _, z]) = z
                    [bz $ boundingBox (rapid (V3 10 10 z) >> approach (V3 2 3 (-4))) | z <- zs] `shouldBe` replicate (length zs) (Boundary (-4.0, -3.5))


        describe "|>|" $ do
                it "places a square next to another square" $
                        (runOperation MF70 defaultCuttingParameters $ square 10 |>| square 15 :: IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ square 10 +^+ translate (V3 (10+3) 0 0) (square 15))
                        -- +3 because of the diameter of the tool
                it "translation of 2 squares placed next to each other" $
                        ((runOperation MF70 defaultCuttingParameters (translate (V3 10 0 (-10) ) (square 10 |>| square 15))) :: IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ translate (V3 10 0 (-10)) (square 10 +^+ translate (V3 (10+3) 0 0) (square 15)))
                        
                it "arc placed next to a diagonal line" $ 
                        (runOperation MF70 defaultCuttingParameters ((rapid (V3 0 0 0) >> feed (V3 10 10 0)) |>| (approach (V3 0 0 0) >> arc CCW (V3 0 5 0) (V3 0 10 0))) :: IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ do
                                rapid (V3 0 0 0)
                                feed (V3 10 10 0)
                                retract 1
                                approach (V3 13 0 0)
                                arc CCW (V3 13 5 0) (V3 13 10 0)
                            )

        describe "|.|" $ do
                it "stacks 2 circular pockets" $
                    (runOperation MF70 defaultCuttingParameters (circularPocket 20 10 0.5 |.| circularPocket 15 10 0.5)::IR) `shouldBe`
                        (runOperation MF70 defaultCuttingParameters $ do
                            circularPocket 20 10 0.5
                            translate (V3 0 0 (-10)) $ circularPocket 15 10 0.5
                        )

