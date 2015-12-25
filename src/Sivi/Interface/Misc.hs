{-|
Module          : Sivi.Interface.Misc
Description     : Miscellaneous functions
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Interface.Misc
(
        withColor
        , showLine
        , showLines
) where

import System.Console.ANSI


withColor :: Color -> IO () -> IO ()
withColor c a = do 
        setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
        a
        setSGR [ SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White ]

cutFill :: Int -> a -> [a] -> [a]
cutFill n x xs = take n $ xs ++ repeat x

showLine :: Int -> String -> String
showLine w = cutFill w ' '

showLines :: Int -> Int -> [String] -> [String]
showLines w h xs = cutFill h (replicate w ' ') (map (showLine w) xs)

