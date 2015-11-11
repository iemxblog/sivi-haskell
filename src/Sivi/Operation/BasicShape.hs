{-|
Module		: Sivi.Operation.BasicShape
Description	: Basic shapes like circle, rectangle, square, ...
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.BasicShape (
	circle
	, circleInner
	, circleOuter
	, circleFromHere
	, cylinderInner
	, cylinderOuter
	, rectangle
	, square
	, centeredRectangle
	, centeredSquare
)
where

import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Operation.Repetition
import Sivi.Backend
import Linear

-- | Circle (does not cut the inside of the circle, but just the contour).
-- Warning !!! No tool radius compensation !
circle :: Backend a => Double 		-- ^ d : Diameter of the circle
	-> Operation a			-- ^ Resulting operation
circle d = approach startingPoint +++ arc CCW (V3 0 0 0) startingPoint
	where startingPoint = V3 (d/2) 0 0

-- | Circle with tool radius compensation on the inner side. Does not cut the inside of the circle, but just the contour.
circleInner :: 	Backend a => Double	-- ^ d : Diameter of the circle
		-> Operation a		-- ^ Resulting operation
circleInner d = getToolDiameter >>= \td -> circle (d-td)

-- | Circle with tool radius compensation on the outer side. Does not cut the inside of the circle, but just the contour.
circleOuter :: 	Backend a => Double 	-- ^ d : Diameter of the circle
		-> Operation a		-- ^ Resulting operation
circleOuter d = getToolDiameter >>= \td -> circle (d+td)

-- | Circle that starts form the current tool position. The diameter is automatically defined by the distance between the origin and the tool position.
circleFromHere :: Backend a => Operation a
circleFromHere = do
			org <- getOrigin
			cp <- getCurrentPosition
			arcNT CCW org cp

-- | Cylinder with tool radius compensation on the inner side. Does not cut the inside of the cylinder, but just the contour.
cylinderInner :: Backend a => Double	-- ^ d : Diameter of the cylinder
		-> Double		-- ^ depth : Depth of the cylinder
		-> Operation a		-- ^ Resulting operation
cylinderInner d depth = zRepetition depth Nothing (const $ circleInner d)

-- | Cylinder with tool radius compensation on the outer side. Does not cut the inside of the cylinder, but just the contour.
cylinderOuter :: Backend a => Double	-- ^ d : Diameter of the cylinder
		-> Double		-- ^ depth : Depth of the cylinder
		-> Operation a		-- ^ Resulting operation
cylinderOuter d depth = zRepetition depth Nothing (const $ circleOuter d)


-- | Rectangle (does not cut the inside of the rectangle, but just the contour).
-- The origin is the bottom left corner.
-- Warning !!! No tool radius compensation !
rectangle :: 	Backend a => Double	-- ^ w : Width of the rectangle (on the x axis)
		-> Double		-- ^ h : height of the rectangle (on the y axis)
		-> Operation a		-- ^ Resulting operation
rectangle w h = approach (V3 0 0 0) +++ feed (V3 w 0 0) +++ feed (V3 w h 0) +++
		feed (V3 0 h 0) +++ feed (V3 0 0 0)

-- | Square.
-- The origin is the bottom left corner.
-- Warning !!! No tool radius compensation !
square :: 	Backend a => Double 	-- ^ a : length of the side of the square
		-> Operation a		-- Resulting operation
square a = rectangle a a

-- | The same as rectangle, but the origin is in the middle of the rectangle.
-- Warning !!! No tool radius compensation !
centeredRectangle :: 	Backend a => Double
			-> Double
			-> Operation a
centeredRectangle w h = translate (V3 (-w/2) (-h/2) 0) $ rectangle w h

-- | The same as square, but the origin is in the center of the square.
-- Warning !!! No tool radius compensation !
centeredSquare :: 	Backend a => Double
			-> Operation a
centeredSquare a = centeredRectangle a a
