module Main (
	main

) where

import Sivi
import Linear

zPlate :: Operation IR
zPlate = noOp

main :: IO ()
main = putStr . (++"M2\n") . toString . runOperationWithDefaultParams $ zPlate
--main = interface . toGCode . runOperationWithDefaultParams $ zPlate
