module ApproxExpectation (
    shouldApprox
) where

import Test.HUnit
import Test.Hspec
import Control.Monad (unless)
import Sivi.Approx

expectTrue :: String -> Bool -> Expectation
expectTrue msg b = unless b (assertFailure msg)

shouldApprox :: (Show a, Approx a) => a -> a -> Expectation
actual `shouldApprox` expected = expectTrue ("expected: " ++ show expected ++ "\n but got: " ++ show actual ++ "\n (Compared with ~=)") (actual ~= expected)
