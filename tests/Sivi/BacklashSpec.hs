module Sivi.BacklashSpec (
	main
) where

import Test.Hspec
import Sivi.Backlash

main :: IO ()
main = hspec $ do
	describe "Backlash" $ do
		it "pouf" $ do
			True `shouldBe` False	
