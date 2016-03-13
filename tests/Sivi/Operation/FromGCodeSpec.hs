module Sivi.Operation.FromGCodeSpec (
        spec
) where

import Test.Hspec
import Linear
import Sivi
import Sivi.IR

spec :: SpecWith ()
spec = describe "fromGCode" $ 
        it "transforms a simple program into an operation" $ do
                let program = 
                        GCode [ g00 {x = Just 10, y = Just 20}
                        , g00 {z = Just 30 }
                        , g01 {x = Just 5, f = Just 100}
                        , g00 {x = Just 2, y = Just 1, z = Just 0}
                        , g03 {x = Just (-2), y = Just (-1), i = Just (-2), j = Just (-1)}
                        , g00 {x = Just 1, y = Just (-1), z = Just 1}
                        , g02 {x = Just (-3), y = Just 1, i = Just (-2), j = Just 1}
                        , g00 {x = Just 4, y = Just 0, z = Just 0}
                        , g38d2 {x = Just 3, f = Just 10}
                        , g92 {x = Just 0}
                        , gcomment { getComment = "Hello, world!" }
                        , m00
                        , g00 {x = Just 1, y = Just 0, z = Just 0}
                        ]
                let expectedOutput = 
                        IR [ Move (V3 10 20 0) Rapid
                        , Move (V3 10 20 30) Rapid
                        , Move (V3 5 20 30) (LinearInterpolation 100)
                        , Move (V3 2 1 0) Rapid
                        , Move (V3 (-2) (-1) 0) (Arc CCW (V3 0 0 0) 100)
                        , Move (V3 1 (-1) 1) Rapid
                        , Move (V3 (-3) 1 1) (Arc CW (V3 (-1) 0 1) 100)
                        , Move (V3 4 0 0) Rapid
                        , Move (V3 3 0 0) (Probe 10)
                        , DefCurPos (V3 0 0 0)
                        , Comment "Hello, world!"
                        , Pause
                        , Move (V3 1 0 0) Rapid
                        ]
                runOperation defaultCuttingParameters (fromGCode program) `shouldBe` expectedOutput
