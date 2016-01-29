module Sivi.Operation.BaseSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR


spec :: SpecWith ()
spec = describe "base operations" $ do

            describe "approach" $ do
                it "makes an approach from a high altitude" $ 
                    (runOperation MF70 defaultCuttingParameters (approach (V3 3 4 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 3 4 50) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach from an altitude between 0 and +dc, from above the destination" $ 
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 0.2) >> approach (V3 0 0 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 0.2) >> slow (V3 0 0 0) >> plunge (V3 0 0 (-0.5)))
                it "makes an approach from an altitude below 0, from above the destination" $ 
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 (-0.2)) >> approach (V3 0 0 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 (-0.2)) >> plunge (V3 0 0 (-0.5)))
                it "makes an approach from an altitude between 0 and +dc" $ 
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 0.2) >> approach (V3 3 4 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 0.2) >> rapid (V3 3 4 0.2) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach from an altitude below 0" $ 
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 (-0.2)) >> approach (V3 3 4 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 0 0 (-0.2)) >> rapid (V3 3 4 (-0.2)) >> plunge (V3 3 4 (-0.5)))
                it "makes a rapid_xy even when destination x == current x" $
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 3 0 30) >> approach (V3 3 4 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 3 0 30) >> rapid (V3 3 4 30) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes a rapid_xy even when destination y == current y" $
                    (runOperation MF70 defaultCuttingParameters (rapid (V3 0 4 30) >> approach (V3 3 4 (-0.5)))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 0 4 30) >> rapid (V3 3 4 30) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach after a translation" $ 
                    (runOperation MF70 defaultCuttingParameters (translate (V3 5 6 (-10)) $ approach (V3 0 0 0))::IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (rapid (V3 5 6 50) >> rapid (V3 5 6 (-9)) >> slow (V3 5 6 (-9.5)) >> plunge (V3 5 6 (-10)))

            describe "arc" $ do
                it "translates an arc" $
                    (runOperation MF70 defaultCuttingParameters (translate (V3 8 9 1) $ arc CCW (V3 0 5 0) (V3 0 10 0)) :: IR) `shouldBe`
                        runOperation MF70 defaultCuttingParameters (arc CCW (V3 8 14 1) (V3 8 19 1))

            describe "zigzag" $ do 
                    it "makes a basic zigzag operation" $ do
                            let path = [
                                    [V3 0 0 0, V3 10 0 0]
                                    , [V3 0 5 0, V3 10 5 0]
                                    , [V3 0 10 0, V3 10 10 0]
                                    , [V3 0 15 0, V3 10 15 0]
                                    , [V3 0 20 0, V3 10 20 0]
                                    ]
                            (runOperation MF70 defaultCuttingParameters {initialPosition = V3 0 0 30} (zigzag path)::IR) `shouldBe`
                                    (runOperation MF70 defaultCuttingParameters $ do
                                        approach (V3 0 0 0)
                                        feed (V3 0 0 0)
                                        feed (V3 10 0 0)
                                        feed (V3 10 5 0)
                                        feed (V3 0 5 0)
                                        feed (V3 0 10 0)
                                        feed (V3 10 10 0)
                                        feed (V3 10 15 0)
                                        feed (V3 0 15 0)
                                        feed (V3 0 20 0)
                                        feed (V3 10 20 0))
                                            
                    it "makes nothing for an empty list" $ do
                            let path = []
                            runOperation MF70 defaultCuttingParameters (zigzag path) `shouldBe` IR []
                    it "makes nothing for a list of empty lists" $ do
                            let path = [[], [], [], []]
                            runOperation MF70 defaultCuttingParameters (zigzag path) `shouldBe` IR []
