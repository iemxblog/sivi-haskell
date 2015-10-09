import Sivi

main :: IO ()
main = do
		s <- readFile "test.ngc"
		case parseGCode s of
			Left err -> error err
			Right gcode -> putStrLn . toString . flatten . runOperationWithDefaultParams . rotate (-90) . fromGCode $ gcode
