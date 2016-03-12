module Sivi.Operation.BaseSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR

runTest :: Operation MF70 IR () -> IR
runTest = runOperation MF70 defaultCuttingParameters


spec :: SpecWith ()
spec = describe "base operations" $ do

            describe "reader monad" $ do
                describe "transformation" $ do
                    it "should run an operation with a transformation" $
                        runTest (withTransformation (\(V3 x y z) -> V3 (x+5) (y+6) (z+3)) $ rapid (V3 1 2 3)) `shouldBe`
                            runTest (rapid (V3 6 8 6))
                    it "should apply a rotation, then a translation" $
                        runTest (rapid (V3 0 0 0) >> translate (V3 1 0 0) (rotate 90 (feed (V3 2 0 0)))) `shouldBe`
                            runTest (rapid (V3 0 0 0) >> feed (V3 1 2 0))
                    it "should apply a translation, then a rotation" $
                        runTest (rapid (V3 0 0 0) >> rotate 90 (translate (V3 1 0 0) (feed (V3 2 0 0)))) `shouldBe`
                            runTest (rapid (V3 0 0 0) >> feed (V3 1 2 0))
                describe "feed rate" $ do
                    it "should modify the feed rate" $
                        runTest (withFeedRate 1234 (feed (V3 1 2 3))) `shouldBe`
                            IR [Move (V3 1 2 3) (LinearInterpolation 1234)]
                    it "should return the feed rate" $
                        getReturnValue MF70 defaultCuttingParameters (withFeedRate 1234 getFeedRate :: Operation MF70 IR Double) `shouldBe` 1234

                describe "plunge rate" $ do
                    it "should modify the plunge rate" $
                        runTest (withPlungeRate 1234 (plunge (V3 1 2 0))) `shouldBe`
                            IR [Move (V3 1 2 0) (LinearInterpolation 1234)]
                    it "should return the plunge rate" $
                        getReturnValue MF70 defaultCuttingParameters (withPlungeRate 1234 getPlungeRate :: Operation MF70 IR Double) `shouldBe` 1234

                describe "probe rate" $ do
                    it "should modify the probe rate" $
                        runTest (withProbeRate 1234 (probe (V3 1 2 0))) `shouldBe`
                            IR [Move (V3 1 2 0) (Probe 1234)]
                    it "should return the probe rate" $
                        getReturnValue MF70 defaultCuttingParameters (withProbeRate 1234 getProbeRate :: Operation MF70 IR Double) `shouldBe` 1234

                describe "depth of cut" $ do
                    it "should modify the depth of cut" $
                        runTest (withDepthOfCut (-1) (zRepetition 5 Nothing (const $ feed (V3 1 0 0)))) `shouldBe`
                            runTest (sequence_ [translate (V3 0 0 (-t)) (feed (V3 1 0 0)) | t <- [1..5]])
                    it "should return the depth of cut" $
                        getReturnValue MF70 defaultCuttingParameters (withDepthOfCut 5 getDepthOfCut :: Operation MF70 IR Double) `shouldBe` 5
                
            describe "state monad" $ do
                describe "position" $ do
                    describe "rapid" $ do
                        it "updates the current position" $
                            getReturnValue MF70 defaultCuttingParameters (rapid (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                        it "updates the current position correctly with a translation" $
                            getReturnValue MF70 defaultCuttingParameters (translate (V3 1 2 3) (rapid (V3 10 17 32)) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 11 19 35)
                    describe "slow" $ do
                        it "updates the current position" $ 
                            getReturnValue MF70 defaultCuttingParameters (slow (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                    describe "feed" $ do
                        it "updates the current position" $ 
                            getReturnValue MF70 defaultCuttingParameters (feed (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                    describe "arc" $ do
                        it "updates the current position" $ 
                            getReturnValue MF70 defaultCuttingParameters (rapid (V3 0 0 0) >> arc CCW (V3 0 0 0) (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                    describe "plunge" $ do
                        it "updates the current position" $ 
                            getReturnValue MF70 defaultCuttingParameters (plunge (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                    describe "probe" $ do 
                        it "updates the current position" $ 
                            getReturnValue MF70 defaultCuttingParameters (probe (V3 10 17 32) >> getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` (V3 10 17 32)
                            

                describe "tool" $ do
                    describe "changeTool" $ do
                        it "changes the current tool" $ 
                            getReturnValue MF70 defaultCuttingParameters (changeTool (EndMill 20 100) >> getTool :: Operation MF70 IR Tool) `shouldBe` EndMill 20 100

            describe "runOperation" $ do
                describe "transformation" $ do
                    it "should run an operation with a transformation" $
                        runOperation MF70 defaultCuttingParameters {transformation = \(V3 x y z) -> V3 (x+5) (y+6) (z+3)} (rapid (V3 1 2 3)) `shouldBe`
                            runTest (rapid (V3 6 8 6))
                describe "feed rate" $ do
                    it "should modify the feed rate" $
                        runOperation MF70 defaultCuttingParameters {Sivi.feedRate = 1234} (feed (V3 1 2 3)) `shouldBe`
                            IR [Move (V3 1 2 3) (LinearInterpolation 1234)]

                describe "plunge rate" $ do
                    it "should modify the plunge rate" $
                        runOperation MF70 defaultCuttingParameters {plungeRate = 1234} (plunge (V3 1 2 0)) `shouldBe`
                            IR [Move (V3 1 2 0) (LinearInterpolation 1234)]

                describe "probe rate" $ do
                    it "should modify the probe rate" $
                        runOperation MF70 defaultCuttingParameters {probeRate = 1234} (probe (V3 1 2 0)) `shouldBe`
                            IR [Move (V3 1 2 0) (Probe 1234)]

                describe "depth of cut" $ do
                    it "should modify the depth of cut" $
                        runOperation MF70 defaultCuttingParameters {depthOfCut = -1} (zRepetition 5 Nothing (const $ feed (V3 1 0 0))) `shouldBe`
                            runTest (sequence_ [translate (V3 0 0 (-t)) (feed (V3 1 0 0)) | t <- [1..5]])

            describe "getReturnValue" $ do
                it "gives the correct transformation" $ do
                    let t = getReturnValue MF70 defaultCuttingParameters {transformation = \(V3 x y z) -> V3 (x+5) (y+4) (z+3)} (getTransformation :: Operation MF70 IR Transformation)
                    t (V3 0 0 0) `shouldBe` V3 5 4 3
                it "gives the correct feed rate" $
                    getReturnValue MF70 defaultCuttingParameters {Sivi.feedRate = 1} (getFeedRate :: Operation MF70 IR Double) `shouldBe` 1
                it "gives the correct plunge rate" $ 
                    getReturnValue MF70 defaultCuttingParameters {plungeRate = 2} (getPlungeRate :: Operation MF70 IR Double) `shouldBe` 2
                it "gives the correct probe rate" $ 
                    getReturnValue MF70 defaultCuttingParameters {probeRate = 3} (getProbeRate :: Operation MF70 IR Double) `shouldBe` 3
                it "gives the correct depth of cut" $ 
                    getReturnValue MF70 defaultCuttingParameters {depthOfCut = -1} (getDepthOfCut :: Operation MF70 IR Double) `shouldBe` -1 
                it "gives the correct position" $ 
                    getReturnValue MF70 defaultCuttingParameters {initialPosition = (V3 5 8 9)} (getCurrentPosition :: Operation MF70 IR (V3 Double)) `shouldBe` V3 5 8 9
                it "gives the correct tool" $ 
                    getReturnValue MF70 defaultCuttingParameters {initialTool = EndMill 20 100} (getTool :: Operation MF70 IR Tool) `shouldBe` EndMill 20 100

            describe "basic moves" $ do
                let ipos = V3 10 3 8
                let testHelperNoTranslation f = 
                        it "doesn't do anything when cp == dst" $
                            runTest (rapid ipos >> f ipos) `shouldBe` runTest (rapid ipos)
                let testHelperTranslated f = 
                        it "doesn't do anything when cp == dst, even when translated" $
                            runTest (translate (V3 1 2 3) (rapid ipos >> f ipos)) `shouldBe` (runTest (translate (V3 1 2 3) $ rapid ipos))
                let testHelper f = testHelperNoTranslation f >> testHelperTranslated f
                describe "rapid" $
                    testHelper rapid
                describe "slow" $
                    testHelper slow
                describe "feed" $
                    testHelper feed
                describe "plunge" $
                    testHelper plunge
                describe "retract" $ do
                    let getz (V3 _ _ z) = z
                    testHelper $ retract . getz 
                describe "rapid_xy" $
                    testHelper rapid_xy
                describe "approach_rapid" $
                    testHelper approach_rapid
                describe "probe" $
                    testHelper probe
                describe "defCurPos" $
                    testHelper defCurPos
                        
            describe "translate" $ do
                it "makes a translated rapid move" $
                    runTest (translate (V3 5 6 7) $ rapid (V3 10 15 20)) `shouldBe`
                        runTest (rapid (V3 15 21 27))
                it "makes a translated slow move" $
                    runTest (translate (V3 5 6 7) $ slow (V3 10 15 20)) `shouldBe`
                        runTest (slow (V3 15 21 27))
                it "makes a translated feed move" $
                    runTest (translate (V3 5 6 7) $ feed (V3 10 15 20)) `shouldBe`
                        runTest (feed (V3 15 21 27))
                it "makes a translated arc move" $ do
                    runTest (translate (V3 5 6 7) $ arc CCW (V3 1 2 3) (V3 4 5 6)) `shouldBe`
                        runTest (arc CCW (V3 6 8 10) (V3 9 11 13))
                it "makes a translated plunge" $ do
                    runTest (translate (V3 5 6 7) $ plunge (V3 10 15 20)) `shouldBe`
                        runTest (plunge (V3 15 21 27))
                it "makes a translated retract" $ do
                    runTest (translate (V3 5 6 7) $ retract 20) `shouldBe`
                        runTest (retract 27)
                it "makes a translated rapid_xy" $ do
                    runTest (translate (V3 5 6 7) $ rapid_xy (V3 10 15 20)) `shouldBe`
                        runTest (rapid_xy (V3 15 21 20))
                it "makes a translated approach_rapid" $ do
                    runTest (translate (V3 5 6 7) $ approach_rapid (V3 10 15 20)) `shouldBe`
                        runTest (approach_rapid (V3 15 21 27))
                
            describe "rapid_xy" $ do
                it "makes a basic rapid_xy move" $
                    runTest (rapid_xy (V3 5 6 0)) `shouldBe`
                        runTest (rapid (V3 5 6 50)) 
                it "doesn't do anything if we are already at destination" $
                    runTest (rapid (V3 5 6 50) >> rapid_xy (V3 5 6 0)) `shouldBe`
                        runTest (rapid (V3 5 6 50))
                it "behaves correctly whith transformations" $
                    runTest (translate (V3 5 6 0) $ rapid_xy (V3 1 1 0)) `shouldBe`
                        runTest (rapid (V3 6 7 50))

            describe "approach" $ do
                it "makes an approach from a high altitude" $ 
                    runTest (approach (V3 3 4 (-0.5))) `shouldBe`
                        runTest (rapid (V3 3 4 50) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach from an altitude between 0 and +dc, from above the destination" $ 
                    runTest (rapid (V3 0 0 0.2) >> approach (V3 0 0 (-0.5))) `shouldBe`
                        runTest (rapid (V3 0 0 0.2) >> rapid (V3 0 0 0.5) >> slow (V3 0 0 0) >> plunge (V3 0 0 (-0.5)))
                it "makes an approach from an altitude below 0, from above the destination" $ 
                    runTest (rapid (V3 0 0 (-0.2)) >> approach (V3 0 0 (-0.5))) `shouldBe`
                        runTest (rapid (V3 0 0 (-0.2)) >> rapid (V3 0 0 0.5) >> slow (V3 0 0 0) >> plunge (V3 0 0 (-0.5)))
                it "makes an approach from an altitude between 0 and +dc" $ 
                    runTest (rapid (V3 0 0 0.2) >> approach (V3 3 4 (-0.5))) `shouldBe`
                        runTest (rapid (V3 0 0 0.2) >> rapid (V3 0 0 0.5) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach from an altitude below 0" $ 
                    runTest (rapid (V3 0 0 (-0.2)) >> approach (V3 3 4 (-0.5))) `shouldBe`
                        runTest (rapid (V3 0 0 (-0.2)) >> rapid (V3 0 0 0.5) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes a rapid_xy even when destination x == current x" $
                    runTest (rapid (V3 3 0 30) >> approach (V3 3 4 (-0.5))) `shouldBe`
                        runTest (rapid (V3 3 0 30) >> rapid (V3 3 4 30) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes a rapid_xy even when destination y == current y" $
                    runTest (rapid (V3 0 4 30) >> approach (V3 3 4 (-0.5))) `shouldBe`
                        runTest (rapid (V3 0 4 30) >> rapid (V3 3 4 30) >> rapid (V3 3 4 0.5) >> slow (V3 3 4 0) >> plunge (V3 3 4 (-0.5)))
                it "makes an approach after a translation" $ 
                    runTest (translate (V3 5 6 (-10)) $ approach (V3 0 0 0)) `shouldBe`
                        runTest (rapid (V3 5 6 50) >> rapid (V3 5 6 (-9)) >> slow (V3 5 6 (-9.5)) >> plunge (V3 5 6 (-10)))
                it "makes an approach from a very low altitude" $ 
                    runTest (rapid (V3 0 0 (-20)) >> approach (V3 1 2 (-2))) `shouldBe`
                        runTest (rapid (V3 0 0 (-20)) >> rapid (V3 0 0 (-1)) >> rapid (V3 1 2 (-1)) >> slow (V3 1 2 (-1.5)) >> plunge (V3 1 2 (-2)))

            describe "arc" $ do
                it "translates an arc" $
                    runTest (translate (V3 8 9 1) $ arc CCW (V3 0 5 0) (V3 0 10 0)) `shouldBe`
                        runTest (arc CCW (V3 8 14 1) (V3 8 19 1))

            describe "zigzag" $ do 
                    it "makes a basic zigzag operation" $ do
                            let path = [
                                    [V3 0 0 0, V3 10 0 0]
                                    , [V3 0 5 0, V3 10 5 0]
                                    , [V3 0 10 0, V3 10 10 0]
                                    , [V3 0 15 0, V3 10 15 0]
                                    , [V3 0 20 0, V3 10 20 0]
                                    ]
                            runTest (rapid (V3 0 0 30) >> zigzag path) `shouldBe`
                                    (runTest $ do
                                        rapid (V3 0 0 30)
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
                            runTest (zigzag path) `shouldBe` IR []
                    it "makes nothing for a list of empty lists" $ do
                            let path = [[], [], [], []]
                            runTest (zigzag path) `shouldBe` IR []
