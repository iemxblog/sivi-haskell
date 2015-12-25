{-|
Module          : Sivi.Misc
Description     : Miscellaneous functions used in several modules
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Misc 
(
        module Sivi.Misc.SharedParsers
        , module Sivi.Misc.Range
        , module Sivi.Misc.ArcInterpolation
        , showDouble
) where

import Sivi.Misc.SharedParsers
import Sivi.Misc.Range
import Sivi.Misc.ArcInterpolation

import Numeric

-- | Helper function used to show a Double with 3 decimals
showDouble :: Double -> String
showDouble d = showFFloat (Just 3) d ""

