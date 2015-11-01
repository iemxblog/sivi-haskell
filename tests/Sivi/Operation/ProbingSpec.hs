module Sivi.Operation.ProbingSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi


spec :: SpecWith ()
spec = 
	describe "Probing operations" $ do 
		describe "probeXMinus" $ 
			it "makes a simple probing operation" $ do
				let l = 10	
				let depth = -10
				let margin = 5
				let td = 3 -- tool diameter
				let pbr = 10 -- probe rate
				runOperation (100, 30, pbr, -0.5) (V3 0 0 0) EndMill{diameter=td, len=42} (probeXMinus (V3 l 0 depth) margin) `shouldBe`
					IR [ Move (V3 (l+td/2+margin) 0 0) Rapid
					, Move (V3 (l+td/2+margin) 0 depth) Rapid
					, Move (V3 (l-td/2-margin) 0 depth) (Probe pbr)
					, DefCurPos (V3 (l+td/2) 0 depth)
					, Move (V3 (l+td/2+margin) 0 depth) Rapid
					]

		describe "probeZMinus" $ do
			it "probes with a ball nose end mill" $ do
				let margin = 5
				let td = 3
				let pbr = 10
				runOperation (100, 30, pbr, -0.5) (V3 1 1 10) BallEndMill{diameter=td, shankDiameter=td, len=42} (probeZMinus (V3 0 0 0) 5) `shouldBe`
					IR [ Move (V3 0 0 10) Rapid
					, Move (V3 0 0 (td/2+margin)) Rapid
					, Move (V3 0 0 (-td/2-margin)) (Probe pbr)
					, DefCurPos (V3 0 0 (td/2))
					, Move (V3 0 0 (td/2+margin)) Rapid
					]
			it "probes with a flat bottom end mill" $ do
				let margin = 5
				let td = 3
				let pbr = 10
				runOperation (100, 30, pbr, -0.5) (V3 1 1 10) EndMill{diameter=td, len=42} (probeZMinus (V3 0 0 0) 5) `shouldBe`
					IR [ Move (V3 0 0 10) Rapid
					, Move (V3 0 0 margin) Rapid
					, Move (V3 0 0 (-margin)) (Probe pbr)
					, DefCurPos (V3 0 0 0)
					, Move (V3 0 0 margin) Rapid
					]
