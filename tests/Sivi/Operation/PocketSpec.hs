module Sivi.Operation.PocketSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = 
        describe "rectangularPocketZigzagP" $ do 
                it "makes a basic rectangular pocket pass" $ 
                        (runOperation MF70 defaultCuttingParameters (rectangularPocketZigzagP 10 20 0.5 False)::IR) `shouldBe`
                            (runOperation MF70 defaultCuttingParameters $ do
                                approach (V3 1.5 1.5 0)
                                feed (V3 1.5 1.5 0)
                                feed (V3 1.5 18.5 0.0)
                                feed (V3 4.0 18.5 0.0)
                                feed (V3 4.0 1.5 0.0)
                                feed (V3 6.5 1.5 0.0)
                                feed (V3 6.5 18.5 0.0)
                                feed (V3 8.5 18.5 0.0)
                                feed (V3 8.5 1.5 0.0)
                                approach (V3 1.5 1.5 0)
                                feed (V3 8.5 1.5 0)
                                feed (V3 8.5 18.5 0)
                                feed (V3 1.5 18.5 0)
                                feed (V3 1.5 1.5 0))
               
                it "makes nothing for an a pocket with a dimension smaller than tool diameter" $ 
                        runOperation MF70 defaultCuttingParameters (rectangularPocketZigzagP 2 10 0.5 False) `shouldBe` IR []
