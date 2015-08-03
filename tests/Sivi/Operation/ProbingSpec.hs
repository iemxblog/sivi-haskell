module Sivi.Operation.ProbingSpec (
	spec
) where

import Test.Hspec
import Linear
import Sivi



spec = describe "probeXMinus" $ do
	it "makes a simple probing operation" $ do
		runOperation  (probeXMinus (V3 l 0 depth) margin) `shouldBe`
			[ Move (V3 (l+td/2+margin) 0 0) Rapid
			, Move (V3 (l+td/2+margin) 0 depth) Rapid
			, Move (V3 (l-td/2-margin) 0 depth) (Probe f)
			, DefCurPos (V3 (l+td/2) 0 depth)
			, Move (V3 (l+td/2+margin) 0 depth) Rapid
			]
	where 
		l = 10	
		depth = -10
		margin = 5
		td = 3 -- defined in runOperation, not very clean....
		f = 100 -- idem
