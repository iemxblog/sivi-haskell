module Sivi.Operation.BoundingBoxSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi


spec :: SpecWith ()
spec = describe "boundingBox" $ do
	it "calculates the bounding box of a circular pocket" $ 
		boundingBox (circularPocket 20 5 0.5) `shouldBe` BoundingBox [Boundary (-10, 10), Boundary (-10, 10), Boundary (-5, 0)]
	it "calculates the bounding box of a translated circular pocket" $ 
		boundingBox (translate (V3 15 20 (-5)) (circularPocket 20 5 0.5)) `shouldBe` BoundingBox [Boundary (5, 25), Boundary (10, 30), Boundary (-10, -5)]
	it "calculates the bounding box of a square" $ 
		boundingBox (square 30) `shouldBe` BoundingBox [Boundary (-1.5, 31.5), Boundary (-1.5, 31.5), Boundary (0, 0)]

	where
		boundingBox = runOperationWithDefaultParams 
