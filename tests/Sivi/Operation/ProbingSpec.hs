module Sivi.Operation.ProbingSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi


spec :: SpecWith ()
spec = describe "probeXMinus" $ 
	it "makes a simple probing operation" $ 
		runOperation (100, 30, pbr, -0.5) (V3 0 0 0) EndMill{diameter=td, len=42} (probeXMinus (V3 l 0 depth) margin) `shouldBe`
			IR [ Move (V3 (l+td/2+margin) 0 0) Rapid
			, Move (V3 (l+td/2+margin) 0 depth) Rapid
			, Move (V3 (l-td/2-margin) 0 depth) (Probe pbr)
			, DefCurPos (V3 (l+td/2) 0 depth)
			, Move (V3 (l+td/2+margin) 0 depth) Rapid
			]
	where 
		l = 10	
		depth = -10
		margin = 5
		td = 3 -- tool diameter
		pbr = 10 -- probe rate
