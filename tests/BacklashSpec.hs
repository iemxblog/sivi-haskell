module BacklashSpec (
	main
) where

import Test.Hspec
import Backlash

main :: IO ()
main = hspec $ do
	describe "Backlash" $ do
		it "pouf" $ do
			True `shouldBe` False	
