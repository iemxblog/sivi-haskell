{-|
Module          : Sivi.Operation.Contour
Description     : Contour operation
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Contour (
        Side(..)
        , contour
)
where

import Linear
import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Backend

data Side = LeftSide | RightSide

-- | Generates a contour with tool radius compensation. 
contour :: Backend a => [V2 Double]     -- ^ path : The list of points of the path
                -> Side                 -- ^ side : Does the tool cut on the left or on the right of the path ?
                -> Bool                 -- ^ cycle : Does the path make a cycle ? yes -> True, False -> No
                -> Operation a          -- ^ The resulting operation
contour [] _ _ = noOp
contour path side cycle = 
        do
                df <- getToolDiameter
                let offsetPath = offset (df/2) side cycle path 
                let firstPoint = v2tov3 $ head offsetPath
                v2Path offsetPath               

-- | Makes the tool radius compensation by generating an offset path.
offset :: Floating a => a       -- ^ d : value of the offset (radius of the current tool)
                -> Side         -- ^ side : direction of the offset
                -> Bool         -- ^ cycle : Does the path make a cycle ? yes -> True, False -> No
                -> [V2 a]       -- ^ path : The path to offset
                -> [V2 a]       -- ^ The resulting path
offset _ _ _ [] = []            
offset d side cycle path = newPath
        where 
                lines = case cycle of
                        True -> zip path (tail path ++ [head path])
                        False -> zip path (tail path)
                vectors = [b-a | (a, b) <- lines]
                offsets = [ d *^ perpNorm v side | v <- vectors]
                newLines = [ (p1+o, p2+o) | ((p1, p2), o) <- zip lines offsets]
                couples = case cycle of
                        True -> zip newLines (tail newLines ++ [head newLines])
                        False -> zip newLines (tail newLines)
                intersections = map (uncurry intersection) couples
                newPath = case cycle of
                                True -> last intersections : intersections 
                                False -> (fst . head $ newLines) : intersections ++ [snd . last $ newLines]

-- | Calculates the intersection of 2 lines.
-- This function is used by 'offset'.
intersection :: Floating a => (V2 a, V2 a)      -- ^ Line 1
                -> (V2 a, V2 a)                 -- ^ Line 2
                -> V2 a                         -- ^ Intersection of line 1 and line 2.
intersection (V2 a b, V2 c d) (V2 e f, V2 g h) = V2 x y
        where
                u = d - b
                v = a - c
                w = a * d - b * c
                u2 = h - f
                v2 = e - g
                w2 = e * h - f * g
                delta = u* v2 - u2 * v                  -- Delta = 0 -> Error !!!!!!!!!!!!!!!!!!!!!!
                x = (w*v2 - w2*v)/delta
                y = (u*w2 - u2*w)/delta

-- | Returns a perpendicular vector, whose norm is 1.
perpNorm :: Floating a => V2 a          -- ^ v : Vector
                        -> Side         -- ^ side : Side of the perpendicular vector : left or right
                         -> V2 a        -- ^ The resulting vector.
perpNorm v side = case side of
                        LeftSide -> o
                        RightSide -> -o
                where
                        o = perp v ^/ norm v

-- | Transforms a (V2 a) into a (V3 a), by appending a 0 for the Z axis.
v2tov3 :: Num a => V2 a 
                -> V3 a
v2tov3 (V2 x y) = V3 x y 0

-- | 2D path operation.
v2Path :: Backend a => [V2 Double]      -- ^ path
        -> Operation a                  -- ^ The resulting operation
v2Path [] = noOp
v2Path (x:xs) = approach (v2tov3 x) >> sequence_ (map (feed . v2tov3) xs)
