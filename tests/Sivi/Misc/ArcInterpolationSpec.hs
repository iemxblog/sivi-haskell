module Sivi.Misc.ArcInterpolationSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi

spec :: SpecWith ()
spec = describe "arcInterpolation" $ 
	it "interpolates a circle in the XY plane" $ do
		let expected = [V3 10 0 0, V3 0 10 0, V3 (-10) 0 0, V3 0 (-10) 0, V3 10 0 0]
		let calculated = arcInterpolation (V3 10 0 0) (V3 10 0 0) (V3 0 0 0) CCW 90
		let approximate x = if abs x < 0.001 then 0 else x
		map (fmap approximate) (zipWith (-) expected calculated)  `shouldBe` replicate 5 (V3 0 0 0)
