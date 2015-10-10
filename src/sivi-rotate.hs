import Sivi
import System.Environment
import System.Exit

parse ["-h"] = usage >> exitWith ExitSuccess
parse [] = usage >> exitWith (ExitFailure 1)
parse [s] = return (read s :: Double)

rotateProgram angle = do
	s <- getContents
	case parseGCode s of
		Left err -> error err
		Right gcode -> putStr . toString . flatten . runOperationWithDefaultParams . rotate angle . fromGCode $ gcode

main :: IO ()
main = getArgs >>= parse >>= rotateProgram

usage = getProgName >>= return . ("Usage: "++) . (++" [-h] angle") >>= putStrLn
