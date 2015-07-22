import Sivi.IR
import Sivi.GCode
import qualified Data.ByteString as B

main :: IO ()
main = do
		s <- B.readFile "test.ngc"
		case parse s of
			Left err -> error $ "Parse error : " ++ err
			Right gcode -> putStrLn . toString . rotateIR (-90) . fromGCode $ gcode
