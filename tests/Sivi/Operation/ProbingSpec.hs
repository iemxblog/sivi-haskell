module Sivi.Operation.ProbingSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = 
	describe "Probing operations" $ do 
		let margin = 5
		let td = 3 -- tool diameter
		let pbr = 10 -- probe rate

		describe "probeXMinus" $ 
			it "makes a simple probing operation" $ do
				let l = 10	
				let depth = -10
				runOperation defaultCuttingParameters (probeXMinus (V3 l 0 depth) margin) `shouldBe`
					IR [ Move (V3 (l+td/2+margin) 0 0) Rapid
					, Move (V3 (l+td/2+margin) 0 depth) Rapid
					, Move (V3 (l-td/2-margin) 0 depth) (Probe pbr)
					, DefCurPos (V3 (l+td/2) 0 depth)
					, Move (V3 (l+td/2+margin) 0 depth) Rapid
					]

		describe "probeZMinus" $ do
			it "probes with a ball nose end mill" $ 
				runOperation defaultCuttingParameters {initialTool = BallEndMill{diameter=td, shankDiameter=td, len=42}, initialPosition = V3 1 1 10} (probeZMinus (V3 0 0 0) 5) `shouldBe`
					IR [ Move (V3 0 0 10) Rapid
					, Move (V3 0 0 (td/2+margin)) Rapid
					, Move (V3 0 0 (-td/2-margin)) (Probe pbr)
					, DefCurPos (V3 0 0 (td/2))
					, Move (V3 0 0 (td/2+margin)) Rapid
					]
			it "probes with a flat bottom end mill" $ 
				runOperation defaultCuttingParameters {initialPosition = V3 1 1 10} (probeZMinus (V3 0 0 0) 5) `shouldBe`
					IR [ Move (V3 0 0 10) Rapid
					, Move (V3 0 0 margin) Rapid
					, Move (V3 0 0 (-margin)) (Probe pbr)
					, DefCurPos (V3 0 0 0)
					, Move (V3 0 0 margin) Rapid
					]
