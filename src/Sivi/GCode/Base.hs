{-|
Module          : Sivi.GCode.Base
Description     : GCode data structure
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.GCode.Base
(
        GCode(..)
        , GCodeInstruction(..)
        , getGCode
        , g00
        , g01
        , g02
        , g03
        , gcomment
        , m00
        , cline
        , g38d2
        , g92
) where

import Data.Monoid
import Linear
import Sivi.Backend
import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Misc

data GCodeInstruction  =
        G00 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double }
        | G01 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double, f :: Maybe Double }
        | G02 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
                i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
        | G03 { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
                i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
        | GComment { getComment :: String }
        | M00
        | CLine { x :: Maybe Double, y :: Maybe Double, z :: Maybe Double,
                i :: Maybe Double, j :: Maybe Double, k :: Maybe Double, f :: Maybe Double }
        | G38d2 { x:: Maybe Double, y :: Maybe Double, z :: Maybe Double, f :: Maybe Double }
        | G92 { x:: Maybe Double, y :: Maybe Double, z :: Maybe Double }
        deriving Eq

newtype GCode = GCode { getgCodeInstructions :: [GCodeInstruction] } deriving Eq

-- | Shows a GCode word (with Maybe value)
gword :: Char -> Maybe Double -> String
gword w (Just v) = w : showDouble v
gword _ Nothing = ""

-- | Compiles parameters of a GCode command
compileParams :: [Char]                 -- ^ List of parameter names
                -> [Maybe Double]       -- ^ List of parameter values
                -> String               -- ^ Compiled parameters
compileParams pn pv = unwords . filter (/= "") $ zipWith gword pn pv

instance Show GCodeInstruction where
        show (G00 mx my mz) = "G00 " ++ compileParams "XYZ" [mx, my, mz]
        show (G01 mx my mz mf) = "G01 " ++ compileParams "XYZF" [mx, my, mz, mf]
        show (G02 mx my mz mi mj mk mf) = "G02 " ++ compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
        show (G03 mx my mz mi mj mk mf) = "G03 " ++ compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
        show (GComment c) = "(" ++ c ++ ")"
        show M00 = "M00"
        show (CLine mx my mz mi mj mk mf) = compileParams "XYZIJKF" [mx, my, mz, mi, mj, mk, mf]
        show (G38d2 mx my mz mf) = "G38.2 " ++ compileParams "XYZF" [mx, my, mz, mf]
        show (G92 mx my mz) = "G92 " ++ compileParams "XYZ" [mx, my, mz]

instance Show GCode where
        show (GCode l) = concatMap ((++"\n") . show) l


instance Monoid GCode where
        mempty = GCode []
        GCode xs `mappend` GCode ys = GCode $ xs ++ ys 

optim :: V3 Double -> V3 Double -> (Maybe Double, Maybe Double, Maybe Double)
optim (V3 x y z) (V3 cx cy cz) = (f x cx, f y cy, f z cz)
        where f d c | d == c = Nothing
                    | otherwise = Just d

instance Backend GCode where
        bRapid dst = do
                cp <- getCurrentPosition
                let (mx, my, mz) = optim dst cp
                if cp == dst
                        then noOp
                        else return $ GCode [G00 mx my mz]

        bFeed fr dst = do
                cp <- getCurrentPosition
                let (mx, my, mz) = optim dst cp
                if cp == dst
                        then noOp
                        else return  $ GCode [G01 mx my mz (Just fr)]   -- fr is not optimized :(

        bArc fr dir center dst = do
                cp <- getCurrentPosition
                let (mx, my, mz) = optim dst cp
                let V3 i j k = center - cp
                let notZero v = if v /= 0 then Just v else Nothing
                case dir of 
                        CW -> return $ GCode [G02 mx my mz (notZero i) (notZero j) (notZero k) (Just fr)]       -- fr is not optimized :(
                        CCW -> return $ GCode [G03 mx my mz (notZero i) (notZero j) (notZero k) (Just fr)]      -- fr is not optimized :(

        bPause = return $ GCode [M00]

        bProbe pbr dst = do
                cp <- getCurrentPosition
                let (mx, my, mz) = optim dst cp
                if cp == dst
                        then noOp
                        else return  $ GCode [G38d2 mx my mz (Just pbr)]

        bDefCurPos p = do
                cp <- getCurrentPosition
                let (mx, my, mz) = optim p cp
                if cp == p
                        then noOp
                        else return $ GCode [G92 mx my mz]

        bComment s = return $ GCode [GComment s]

        bName _ op = op -- name is ignored in this instance

-- | Returns the GCode generated from an operation. This is a GCode specific version of 'runOperation'.
getGCode ::     CuttingParameters
                -> Operation GCode              -- ^ op : Operation to tun
                -> GCode                        -- ^ Resulting GCode program
getGCode = runOperation

-- | Smart constructor for 'G00'
g00 :: GCodeInstruction
g00 = G00 Nothing Nothing Nothing

-- | Smart constructor for 'G01'
g01 :: GCodeInstruction
g01 = G01 Nothing Nothing Nothing Nothing

-- | Smart constructor for 'G02'
g02 :: GCodeInstruction
g02 = G02 Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Smart constructor for 'G03'
g03 :: GCodeInstruction
g03 = G03 Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Smart constructor for 'GComment'
gcomment :: GCodeInstruction
gcomment = GComment ""

-- | Smart constructor for M00
m00 :: GCodeInstruction
m00 = M00

-- | Smart constructor for 'CLine'
cline :: GCodeInstruction
cline = CLine Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Smart constructor for 'G38d2'
g38d2 :: GCodeInstruction
g38d2 = G38d2 Nothing Nothing Nothing Nothing

-- | Smart constructor for 'G92'
g92 :: GCodeInstruction
g92 = G92 Nothing Nothing Nothing
