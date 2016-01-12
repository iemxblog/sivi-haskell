import Sivi
import System.Environment
import System.Exit
import Control.Monad

parse ["-h"] = usage >> exitSuccess
parse [] = usage >> exitWith (ExitFailure 1)
parse [s] = return (read s :: Double)

rotateProgram angle = do
        s <- getContents
        case parseGCode s of
                Left err -> error err
                Right gcode -> print . getGCode MF70 defaultCuttingParameters . rotate angle . fromGCode $ gcode

main :: IO ()
main = getArgs >>= parse >>= rotateProgram

usage = liftM (("Usage: "++) . (++" [-h] angle")) getProgName >>= putStrLn
