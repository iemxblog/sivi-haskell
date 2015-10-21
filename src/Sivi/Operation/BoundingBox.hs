{-|
Module		: Sivi.Operation.BoundingBox
Description	: Bounding box of an operation
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Operation.BoundingBox(
	Boundary(..)
	, BoundingBox(..)	
	, (|>|)
) where

import Data.Monoid
import Linear
import Control.Monad
import Sivi.Backend
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Misc.ArcInterpolation

data Boundary = Boundary (Double, Double) 
		| EmptyBoundary
		deriving (Eq, Show)


newtype BoundingBox = BoundingBox [Boundary] deriving (Eq, Show)

instance Monoid Boundary where
	mempty = EmptyBoundary
	EmptyBoundary `mappend` b = b
	b `mappend` EmptyBoundary = b
	Boundary (m1, p1) `mappend` Boundary (m2, p2) = Boundary (min m1 m2, max p1 p2)

instance Monoid BoundingBox where
	mempty = BoundingBox (replicate 3 mempty)
	BoundingBox xs1 `mappend` BoundingBox xs2 = BoundingBox $ zipWith mappend xs1 xs2

instance Backend BoundingBox where
	bRapid (V3 x y z) = do
		td <- getToolDiameter	
		return $ BoundingBox [Boundary (x-td/2, x+td/2), Boundary (y-td/2, y+td/2), Boundary (z, z)]
	bFeed _ (V3 x y z) = do
		td <- getToolDiameter	
		return $ BoundingBox [Boundary (x-td/2, x+td/2), Boundary (y-td/2, y+td/2), Boundary (z, z)]
	bArc f dir center dst = do
		cp <- getCurrentPosition
		let points = arcInterpolation cp dst center dir 1
		liftM mconcat $ mapM (bFeed f) points

		
	bPause = return mempty

	bProbe _ _ = return mempty

	bDefCurPos _ = return mempty
	
	bComment _ = return mempty

	bName _ op = op


--(|>|) :: Backend a => Operation a -> Operation a -> Operation a
op1 |>| op2 = do
	BoundingBox [bx1, _, _] <- op1
	BoundingBox [bx2, _, _] <- op2
	case (bx1, bx2) of
		(EmptyBoundary, _) -> op1 +++ op2
		(Boundary (_, xp1), EmptyBoundary) -> op1 +++ translate (V3 xp1 0 0) op2
		(Boundary (_, xp1), Boundary (xm2, _)) -> op1 +++ translate (V3 (xp1-xm2) 0 0) op2

--boundingBox :: Backend a => Operation a -> Operation BoundingBox
--boundingBox op = 
-- correct bName in plotter.hs !!!!!!
