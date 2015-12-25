{-|
Module          : Sivi.Misc.ArcInterpolation
Description     : Interpolation of arcs
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Misc.ArcInterpolation
(
        arcInterpolation
) where

import Linear
import Sivi.Misc.Range
import Sivi.Operation.Types


-- | Function that interpolates an arc. Circles are possible only in the XY plane (quick fix), because it is not possible to know in which plane they are (the 3 points used to define them are colinear). Arcs are possible in 3D if the starting, ending and center are not colinear.
arcInterpolation :: V3 Double   -- ^ from : Starting point
                -> V3 Double    -- ^ to : End point
                -> V3 Double    -- ^ cen : Center of the arc
                -> ArcDirection -- ^ dir : Direction ('CW' : clockwise, or 'CCW' : counterclockwise)
                -> Double       -- ^ ai : Angle increment (in degrees)
                -> [V3 Double]  -- ^ List of points
arcInterpolation from to cen dir ai = [o + rotate (axisAngle axis angle) oa | angle <- angles]
        where   
                a = from
                b = to
                o = cen
                oa = a - o
                ob = b - o
                axisTmp = cross oa ob   -- temporary fix for circles...
                axis = case axisTmp of
                        V3 0 0 0 -> V3 0 0 1
                        _ -> axisTmp
                alpha = atan2 (norm (cross oa ob)) (dot oa ob)
                alpha' = case dir of
                        CW -> -2 * pi - alpha
                        CCW -> case alpha of
                                        0 -> 2*pi
                                        _ -> alpha
                step = case dir of
                        CW -> negate ai * pi / 180
                        CCW -> ai * pi / 180
                angles = range 0 alpha' step

