module Main (
	main
) where

import IR
import Operation.Repetition
import Operation.Misc
import Operation.Base

main :: IO ()
main = putStr . compile . runOperation $ zRepetition (-10000) (-0.5) (Just 1) (saw_left 10)
