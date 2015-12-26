module Sivi.OpenSCADSpec (
        spec    
) where

import Test.Hspec
import Linear
import Sivi.OpenSCAD

        
spec :: SpecWith ()
spec = describe "OpenSCAD backend" $ do
                let objects = [
                                EmptyObject
                                , Sphere 1.5
                                , Hull [Sphere 1.5, Translate (V3 10 0 0) (Sphere 1.5)]
                                , Union [Sphere 1.5, Translate (V3 1 0 0) (Sphere 3)]
                                , Translate (V3 10 20 30) (Sphere 1.5)
                                ]
                it "should verify the first monoid law" $ 
                        map (mempty `mappend`) objects `shouldBe` objects

                it "should verify the second monoid law" $ 
                        map (`mappend` mempty) objects `shouldBe` objects

                it "should verify the third monoid law" $ 
                        all (==True) [(x `mappend` y) `mappend` z == x `mappend` (y `mappend` z) | x <- objects, y <- objects, z <- objects] `shouldBe` True

