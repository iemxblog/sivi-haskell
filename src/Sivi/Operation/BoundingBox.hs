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
) where

import Data.Monoid

newtype Boundary = Boundary (Double, Double) deriving (Eq, Show)

newtype BoundingBox = BoundingBox [Boundary] deriving (Eq, Show)

instance Monoid Boundary where
	mempty = Boundary (0, 0)
	Boundary (m1, p1) `mappend` Boundary (m2, p2) = Boundary (min m1 m2, max p1 p2)

instance Monoid BoundingBox where
	mempty = BoundingBox (replicate 3 mempty)
	BoundingBox xs1 `mappend` BoundingBox xs2 = BoundingBox $ zipWith mappend xs1 xs2

instance Backend BoundingBox where
	bRapid (V3 x y z) = do
		td <- getToolDiameter	
		return $ BoundingBox [Boundary (x-td/2, x+td/2), Boundary (y-td/2, y+td/2), (z, z)]
	bFeed _ (V3 x y z) = do
		td <- getToolDiameter	
		return $ BoundingBox [Boundary (x-td/2, x+td/2), Boundary (y-td/2, y+td/2), (z, z)]
	bArc _ dir center dst = do
		
	bPause = mempty

	bDefCurPos =
	
	bComment =

	bName = 
