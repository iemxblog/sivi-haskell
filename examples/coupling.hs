module Main (
	main
) where

import Sivi
import Linear

coupling :: Operation IRTree
coupling = 	chain 5 [
			probeOuterCylinder 20 5 (ProbeTool 3 42)
			, cylinderInner 4.5 7
		]

main :: IO ()
--main = putStr . (++"M2\n") . toString . flatten . runOperationWithDefaultParams $ coupling
main = interface . toGCode . flatten . runOperationWithDefaultParams $ coupling
