module Operation.Misc (
	saw_left
	, drill
)
where
import IR
import Linear
import Operation.Base

saw_left :: Double
	-> Operation IR
saw_left w = do
		df <- getToolDiameter
		op1 <- approach $ V3 (-df/2) (-df/2) 0 
		op2 <- feed $ V3 (-df/2) (w+df/2) 0
		return (op1 ++ op2)

drill :: Operation IR
drill = approach (V3 0 0 0)
