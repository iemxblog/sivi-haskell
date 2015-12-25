{-|
Module          : Sivi.Operation.FromGCode
Description     : Conversion of GCode to Operation
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
module Sivi.Operation.FromGCode
(
        fromGCode
) where

import Control.Monad.State
import qualified Data.Map as Map
import Linear
import Sivi.GCode
import Sivi.Operation.Base
import Sivi.Operation.Types
import Sivi.Backend

-- | Monadic datatype to memorize GCode commands (as String) and parameters (as Data.Map.Map Char Double)
type GCodeTransformer a = State (String, Map.Map Char Double) a

getParamsMap :: GCodeTransformer (Map.Map Char Double)
getParamsMap = liftM snd get

getCommand :: GCodeTransformer String
getCommand = liftM fst get

putParam :: Char -> Maybe Double -> GCodeTransformer ()
putParam _ Nothing = return ()
putParam pn (Just v) = do
                        (c, ps) <- get
                        put (c, Map.insert pn v ps)

putParams :: [Char] -> [Maybe Double] -> GCodeTransformer ()
putParams pns pvs = mapM_ (uncurry putParam) $ zip pns pvs

putCommand :: String -> GCodeTransformer ()
putCommand c = do
                ps <- getParamsMap
                put (c, ps)

getParam :: Char -> GCodeTransformer Double
getParam pn = do
                ps <- getParamsMap
                case Map.lookup pn ps of
                        Nothing -> error $ "getParam : Unknow parameter " ++ [pn]       
                        Just pv -> return pv

getParams :: [Char] -> GCodeTransformer [Double]
getParams = mapM getParam
                        

-- | Transforms a 'GCode' instruction into an 'Operation'
fromGCode' :: Backend a => GCodeInstruction -> GCodeTransformer (Operation a)
fromGCode' (G00 x y z) = do
                                putParams "XYZ" [x,y,z]
                                putCommand "G00"
                                [x',y',z'] <- getParams "XYZ"
                                return $ rapid (V3 x' y' z')
fromGCode' (G01 x y z f) = do
                                putParams "XYZF" [x,y,z,f]
                                putCommand "G01"
                                [x',y',z',f'] <- getParams "XYZF"
                                return $ withFeedRate f' (feed (V3 x' y' z'))
fromGCode' (G02 x y z i j k f) = do
                                        [px, py, pz] <- getParams "XYZ" -- previous position
                                        putParams "XYZIJKF" [x,y,z,i,j,k,f]
                                        putCommand "G02"
                                        [x',y',z',i',j',k',f'] <- getParams "XYZIJKF"
                                        let center = V3 px py pz + V3 i' j' k'
                                        let dst = V3 x' y' z'
                                        return $ withFeedRate f' (arc CW center dst)
fromGCode' (G03 x y z i j k f) = do
                                        [px, py, pz] <- getParams "XYZ" -- previous position
                                        putParams "XYZIJKF" [x,y,z,i,j,k,f]
                                        putCommand "G03"
                                        [x',y',z',i',j',k',f'] <- getParams "XYZIJKF"
                                        let center = V3 px py pz + V3 i' j' k'
                                        let dst = V3 x' y' z'
                                        return $ withFeedRate f' (arc CCW center dst)
fromGCode' (Sivi.GCode.GComment s) = return $ comment s
fromGCode' M00 = return pause
fromGCode' (G38d2 x y z f) = do
                                putParams "XYZF" [x,y,z,f]
                                putCommand "G38.2"
                                [x',y',z',f'] <- getParams "XYZF"
                                return $ withProbeRate f' (probe (V3 x' y' z'))
fromGCode' (G92 x y z) = do
                                putParams "XYZ" [x, y, z]
                                -- no putCommand !
                                [x',y',z'] <- getParams "XYZ"
                                return $ defCurPos (V3 x' y' z')
fromGCode' (CLine x y z i j k f) = do
                                        c <- getCommand
                                        case c of
                                                "G00" -> fromGCode' (G00 x y z)         
                                                "G01" -> fromGCode' (G01 x y z f)
                                                "G02" -> fromGCode' (G02 x y z i j k f)
                                                "G03" -> fromGCode' (G03 x y z i j k f)
                                                "G38.2" -> fromGCode' (G38d2 x y z f)
                                                _ -> error $ "fromGCode' : Unknown memorized command (" ++ c ++ ")"

-- | Transforms GCode to an 'Operation'
fromGCode :: Backend a => GCode -> Operation a
fromGCode (GCode xs) = opsequence operations
        where
                operations = evalState (mapM fromGCode' xs) ("", Map.fromList (zip "XYZIJKF" (repeat 0)))
