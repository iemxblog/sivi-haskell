{-|
Module          : Sivi.Approx
Description     : Approximation of floating point numbers
Copyright       : (c) Maxime ANDRE, 2016
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}

module Sivi.Approx (
    Approx(..)
) where

import Linear

class Approx n where
    (~=) :: n -> n -> Bool

-- | Comparison of doubles with epsilon = 0.0001.
-- Why 0.0001 ? Because it is the least input increment of the majority of CNC machines that work with inches.
-- And it is below the least input increment of machines that work in millimeters (0.001).
instance Approx Double where
    a ~= b = abs (b-a) < 0.0001

instance Approx n => Approx (V3 n) where
    (V3 a1 b1 c1) ~= (V3 a2 b2 c2) = (a1 ~= a2) && (b1 ~= b2) && (c1 ~= c2)

instance Approx n => Approx [n] where
    [] ~= [] = True
    [] ~= _ = False
    _ ~= [] = False
    (x1:xs1) ~= (x2:xs2) = (x1 ~= x2) && (xs1 ~= xs2)
