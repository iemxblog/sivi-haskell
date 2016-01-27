{-|
Module          : Sivi.Operation.BoundingBox
Description     : Bounding box of an operation
Copyright       : (c) Maxime ANDRE, 2015
License         : GPL-2
Maintainer      : iemxblog@gmail.com
Stability       : experimental
Portability     : POSIX
-}
{-# LANGUAGE RankNTypes #-}
module Sivi.Operation.BoundingBox(
        Boundary(..)
        , BoundingBox(..)       
        , (|>|)
        , (|.|)
) where

import Data.Monoid
import Linear
import Control.Monad
import Control.Monad.RWS
import Sivi.Backend
import Sivi.Operation.Types
import Sivi.Operation.Base
import Sivi.Misc.ArcInterpolation

data Boundary = Boundary (Double, Double) 
                | EmptyBoundary
                deriving (Show)

instance Eq Boundary where  -- useful for tests
        EmptyBoundary == EmptyBoundary = True
        EmptyBoundary == Boundary _ = False
        Boundary _ == EmptyBoundary = False
        Boundary (a, b) == Boundary (c, d) = all (<0.001) [abs (c-a), abs (d-b)]

newtype BoundingBox = BoundingBox [Boundary] deriving (Eq, Show)

instance Monoid Boundary where
        mempty = EmptyBoundary
        EmptyBoundary `mappend` b = b
        b `mappend` EmptyBoundary = b
        Boundary (m1, p1) `mappend` Boundary (m2, p2) = Boundary (min m1 m2, max p1 p2)

instance Monoid BoundingBox where
        mempty = BoundingBox (replicate 3 mempty)
        BoundingBox xs1 `mappend` BoundingBox xs2 = BoundingBox $ zipWith mappend xs1 xs2


-- | Bounding boxes size depends only on feeds.
-- For a flat operation, like 'rectangle', the depth is given by the feed that appends during the 'approach' move.
instance Backend BoundingBox where
        bRapid _ = return ()
        bFeed _ (V3 x2 y2 z2) = do
                td <- getToolDiameter   
                dc <- getDepthOfCut
                V3 x1 y1 z1 <- getCurrentPosition
                tell $ BoundingBox [Boundary (x1-td/2, x1+td/2), Boundary (y1-td/2, y1+td/2), Boundary (z1, z1)]
                tell $ BoundingBox [Boundary (x2-td/2, x2+td/2), Boundary (y2-td/2, y2+td/2), Boundary (z2, z2)]
        bArc f dir center dst = do
                cp <- getCurrentPosition
                let points = arcInterpolation cp dst center dir 1
                mapM_ (bFeed f) points

                
        bPause = return ()

        bProbe _ _ = return ()

        bDefCurPos _ = return ()
        
        bComment _ = return ()

        bName _ op = op


calculateBoundingBoxes :: Backend w => (forall w . Backend w => Operation m w ())   -- ^ op1 : Operation 1
        -> (forall w . Backend w => Operation m w ())                               -- ^ op2 : Operation 2
        -> Operation m w (BoundingBox, BoundingBox)                                         
calculateBoundingBoxes op1 op2 = do
        environment <- ask
        state1 <- get
        let (state2, b1) = execRWS op1 environment state1
        let (_, b2) = execRWS op2 environment state2
        return (b1, b2)


-- | Place an operation next to another, on the X axis.
(|>|) :: Backend w => (forall w . Backend w => Operation m w ())    -- ^ op1 : Operation 1
        -> (forall w . Backend w => Operation m w ())               -- ^ op2 : Operation 2
        -> Operation m w ()                                         -- ^ Operation 1, retract at z=1, Operation 2 next to Operation 1 (on its right)
op1 |>| op2 = do
        (BoundingBox [bx1, _, _], BoundingBox [bx2, _, _]) <- calculateBoundingBoxes op1 op2
            
        case (bx1, bx2) of
                (EmptyBoundary, _) -> op1 +^+ op2
                (Boundary (_, xp1), EmptyBoundary) -> op1 +^+ translate (V3 xp1 0 0) op2
                (Boundary (_, xp1), Boundary (xm2, _)) -> op1 +^+ translate (V3 (xp1-xm2) 0 0) op2


-- | Stacks two operations (the second under the other)
(|.|) :: Backend w =>
        (forall w . Backend w => Operation m w ())      -- ^ op1 : Operation 1
        -> (forall w . Backend w => Operation m w ())   -- ^ op2 : Operation 2
        -> Operation m w ()                             -- ^ Operation 1, then Operation 2 under it
op1 |.| op2 = do
        (BoundingBox [_, _, bz1], BoundingBox [_, _, bz2]) <- calculateBoundingBoxes op1 op2  
        case (bz1, bz2) of
                (EmptyBoundary, EmptyBoundary) -> op1 >> op2
                (EmptyBoundary, Boundary (_, zp2)) -> op1 >> translate (V3 0 0 (-zp2)) op2
                (Boundary (zm1, _), EmptyBoundary)-> op1 >> translate (V3 0 0 zm1) op2
                (Boundary (zm1, _), Boundary (_, zp2)) -> op1 >> translate (V3 0 0 (zm1-zp2)) op2
