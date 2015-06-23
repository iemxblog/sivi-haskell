{-|
Module		: Operation
Description	: Composable operations to build a CNC program
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Operation(
	module Operation.Base
	, module Operation.Repetition
	, module Operation.Pocket
	, module Operation.Misc
) where

import Operation.Base
import Operation.Repetition
import Operation.Pocket
import Operation.Misc
