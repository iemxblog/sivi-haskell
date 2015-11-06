{-|
Module		: Sivi.OpenSCAD
Description	: Export toolpath to OpenSCAD format
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.OpenSCAD
(
	OSObject(..)
	, simulation
) where

import Linear
import Sivi.Backend
import Sivi.Operation.Types
import Sivi.Operation.Base
import Data.Char
import Control.Monad
import Data.Monoid
import Sivi.Misc.ArcInterpolation

oV3 :: Show a => V3 a -> String
oV3 (V3 x y z) = "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ "]"

data OSObject = 
	EmptyObject
	| Sphere Double -- ^ r
	| Cylinder Double Double Double Bool -- ^ h r1 r2 center
	| Hull [OSObject]
	| Union [OSObject]
	| Difference [OSObject]
	| Translate (V3 Double) OSObject	
	| Rotate (V3 Double) OSObject
	deriving Eq	

instance Show OSObject where
	show EmptyObject = ""
	show (Sphere r) = "sphere(r=" ++ show r ++ ");\n"
	show (Cylinder h r1 r2 center) = "cylinder(h=" ++ show h ++", r1=" ++ show r1 ++ ", r2=" ++ show r2 ++ ", center=" ++ map toLower (show center) ++ ");\n"
	show (Hull o) = "hull() {\n" ++ concatMap show o ++ "}\n"
	show (Union o) = "union() {\n" ++ concatMap show o ++ "}\n"
	show (Difference o) = "difference() {\n" ++ concatMap show o ++ "}\n"
	show (Translate v o) = "translate(" ++ oV3 v ++ ") {\n" ++ show o ++ "}\n"
	show (Rotate v o) = "rotate(" ++ oV3 v ++ ") {\n" ++ show o ++ "}\n"

toolShape :: Tool -> OSObject
toolShape (EndMill d l) = Cylinder l (d/2) (d/2) False
toolShape (BallEndMill d sd l) = Union [Cylinder l (sd/2) (sd/2) False, Sphere (d/2)]
toolShape (ProbeTool _ _) = EmptyObject

instance Monoid OSObject where
	mempty = EmptyObject

	EmptyObject `mappend` o = o
	o `mappend` EmptyObject = o

	Union xs `mappend` Union ys = Union (xs ++ ys)
	Union xs `mappend` o2 = Union (xs ++ [o2])
	o1 `mappend` Union xs = Union (o1 : xs)
	o1 `mappend` o2 = Union [o1, o2]
	

instance Backend OSObject where
	bRapid _ = return mempty	
	bFeed _ dst = do
		cp <- getCurrentPosition
		ts <- liftM toolShape getTool
		return $ Hull [Translate cp ts, Translate dst ts]

	bArc fr dir cen dst = do
		cp <- getCurrentPosition
		opsequence [bFeed fr p | p <- arcInterpolation cp dst cen dir 1]

	bPause = return mempty

	bProbe _ _ = return mempty

	bDefCurPos _ = return mempty

	bComment _ = return mempty

	bName _ op = op

simulation :: OSObject -> Operation OSObject -> String
simulation raw op = show $ Difference [raw, runOperationWithDefaultParams op]
