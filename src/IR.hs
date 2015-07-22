{-|
Module		: IR
Description	: Intermediate representation of a CNC program
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module IR 
(
	module IR.Base
	, module IR.ToGCode
	, module IR.FromGCode
	, module IR.Transformation
) where

import IR.Base
import IR.ToGCode
import IR.FromGCode
import IR.Transformation


