{-|
Module          : Sivi.Operation.pocket
Description     : Pocketing operations (circular pocket, rectangular pocket, ...)
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.Pocket (
        circularPocket
        , circularPocketP
        , rectangularPocket
        , rectangularPocketP
        , rectangularPocketZigzagP      
        , rectangularPocketZigzag
)
where

import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Operation.BasicShape
import Sivi.Operation.Repetition
import Sivi.Backend
import Sivi.Misc.Range
import Linear
import Data.Monoid

-- | Generates an archimedean spiral. (used in 'circularPocket')
-- Increment in angle is +1 degree. 
archimedeanSpiral ::    Backend w => Double     -- ^ d : External diameter of the spiral
                        -> Double               -- ^ step_over : the end mill covers step_over mm of the precedent turn
                        -> Operation m w ()     -- ^ Resulting operation
archimedeanSpiral d step_over = 
        do
                approach (V3 0 0 0) 
                df <- getToolDiameter
                let angle = [i * pi / 180 | i <- [0..]] -- angle = [1 degree, 2 degrees, ...]
                let radius = takeWhile (\r -> r+df/2 < d/2) [a * (df - step_over) / (2 * pi) | a <- angle]
                let lastPoint = ((d-df)/2, pi*(d-df)/(df-step_over))  -- So at the end we are at the exact radius (d-df)/2
                sequence_ [feed (V3 (r*cos a) (r*sin a) 0) | (r, a) <- zip radius angle ++ [lastPoint]]

-- | Generates a single pass of a circular pocket.
circularPocketP :: Backend w => Double          -- ^ d : Diameter of the pocket
                -> Double                       -- ^ step_over : The end mill covers step_over mm of the precedent turn (in the spiral)
                -> Operation m w ()             -- ^ Resulting operation
circularPocketP d step_over = do
                                approach (V3 0 0 0)
                                archimedeanSpiral d step_over
                                circleFromHere -- the spiral ends at radius = d-df/2, so we start a circle from here

-- | Generates a circular pocket.
circularPocket :: Backend w => Double           -- ^ d : Diameter of the pocket
                -> Double                       -- ^ depth : Depth of the pocket
                -> Double                       -- ^ step_over : The end mill covers step_over mm of the precedent turn (in the spiral)
                -> Operation m w ()             -- ^ Resulting operation
circularPocket d depth step_over = zRepetition depth Nothing (const $ circularPocketP d step_over)
                                

-- | Generates the coordinates of a rectangular spiral (in 2D).
-- Recursive function which generates a piece of spiral, then calls itself to generate a bigger piece.
rectangularSpiralR :: (Num a, Integral b) => (a, a)     -- ^ (xo, yo) : Coordinates of the origin of the spiral
                        -> b                            -- ^ i : Multiplicator coefficient to generate bigger and bigger pieces 
                        -> a                            -- ^ sx : Spacing between each turn for the x axis
                        -> a                            -- ^ sy : Spacing between each turn for the y axis
                        -> [(a, a)]                     -- ^ Resulting rectangular spiral
rectangularSpiralR (xo, yo) i sx sy  = 
                [(xo, yo), (xo+i'*sx, yo), (xo+i'*sx, yo+i'*sy), (xo-sx, yo+i'*sy)] ++ rectangularSpiralR (xo-sx, yo-sy) (i+2) sx sy
                where
                        i' = fromIntegral i

-- | Interface for 'rectangularSpiralR'
rectangularSpiral :: Num a => 
                        a                               -- ^ sx : Spacing betwenn each turn for the x axis
                        -> a                            -- ^ sy : Spacing between each turn for the y axis
                        -> [(a, a)]                     -- ^ Resulting rectangular spiral
rectangularSpiral = rectangularSpiralR (0, 0) 1

-- | Generates a single pass of a rectangular pocket. The P means pass.
rectangularPocketP ::   Backend w => Double                             -- ^ lx : Size of the pocket on the x axis
                        -> Double                       -- ^ ly : Size of the pocket on the y axis
                        -> Double                       -- ^ step_over : The end mill covers step_over mm of the precedent turn
                        -> Operation m w ()             -- Resulting operation
rectangularPocketP lx ly step_over = do
                        td <- getToolDiameter           
                        if td > lx || td > ly 
                                then noOp
                                else do
                                        approach (V3 0 0 0)
                                        let initial_spacing = td - step_over
                                        let cycles_x = floor $ (lx - td)/(2 * initial_spacing)
                                        let cycles_y = floor $ (ly - td)/(2 * initial_spacing)
                                        let cycles = fromIntegral $ max cycles_x cycles_y
                                        let spacing_x = (lx - td)/(2 * cycles)
                                        let spacing_y = (ly - td)/(2 * cycles)
                                        let sp' = takeWhile (\(x,y) -> abs x <= lx/2 && abs y <= ly/2) $ rectangularSpiral spacing_x spacing_y
                                        sequence_ [feed (V3 x y 0) | (x, y) <- sp']
                                        centeredRectangle (lx-td) (ly-td)

-- | Generates a rectangular pocket. (spiral version)
rectangularPocket ::    Backend w => Double             -- ^ lx : Size of the pocket on the x axis
                        -> Double                       -- ^ ly : Size of the pocket on the y axis
                        -> Double                       -- ^ depth : Depth of the pocket
                        -> Double                       -- ^ step_over : Then end mill covers step_over mm of the precedent turn
                        -> Operation m w ()             -- ^ Resulting operation
rectangularPocket lx ly depth step_over = zRepetition depth Nothing (const $ rectangularPocketP lx ly step_over)

-- | Generates a single pass of a rectangular pocket (zigzag version).
rectangularPocketZigzagP :: Backend w =>
                        Double          -- ^ lx : Size of the pocket on the x axis
                        -> Double               -- ^ ly : Size of the pocket on the y axis
                        -> Double               -- ^ stepOver : The end mill covers stepOver mm of the precedent turn
                        -> Bool                 -- ^ center : True -> centered, False -> not centered
                        -> Operation m w ()
rectangularPocketZigzagP lx ly stepOver center = do
                td <- getToolDiameter
                if td > lx || td > ly
                        then noOp
                        else do
                                -- d1 is the longest side, d2 the shortest
                                -- d1v is the direction of the longest side (x or y axis), d2v is the direction of the shortest
                                let (d1, d2, d1v, d2v) = case compare lx ly of
                                        LT -> (ly, lx, V3 0 1 0, V3 1 0 0)
                                        EQ -> (lx, ly, V3 1 0 0, V3 0 1 0)
                                        GT -> (lx, ly, V3 1 0 0, V3 0 1 0)
                                let translation = case center of
                                        True -> V3 (-lx/2) (-ly/2) 0
                                        False -> V3 0 0 0
                                translate translation $ zigzag [[d1v ^* (td/2) + d2v^*a, d1v ^* (d1-td/2) + d2v^*a] | a <- range (td/2) (d2-td/2) (td-stepOver)] >> translate (V3 (td/2) (td/2) 0 ) (rectangle (lx-td) (ly-td))

-- | Generates a rectangular pocket. (zigzag version)
rectangularPocketZigzag ::      Backend w => Double             -- ^ lx : Size of the pocket on the x axis
                                -> Double                       -- ^ ly : Size of the pocket on the y axis
                                -> Double                       -- ^ depth : Depth of the pocket
                                -> Double                       -- ^ step_over : Then end mill covers step_over mm of the precedent turn
                                -> Bool                         -- ^ center : True -> centered, False -> not centered
                                -> Operation m w ()             -- ^ Resulting operation
rectangularPocketZigzag lx ly depth step_over center = zRepetition depth Nothing (const $ rectangularPocketZigzagP lx ly step_over center)

