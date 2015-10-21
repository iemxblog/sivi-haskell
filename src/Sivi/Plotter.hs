{-|
Module		: Sivi.Plotter
Description	: Plot an operation with a graphical interface
Copyright	: (c) Maxime ANDRE, 2015
License		: GPL-2
Maintainer	: iemxblog@gmail.com
Stability	: experimental
Portability	: POSIX
-}
module Sivi.Plotter
(
	plot
	, drawOperation
	, Canvas
	, initCanvas
) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix hiding (scale)
import Linear
import Data.Monoid
import Sivi.Operation.Types
import Sivi.Operation.Base hiding (arc)
import Sivi.Backend
import Sivi.Misc.ArcInterpolation

type Projection = V3 Double -> V2 Double

newtype Drawing = Drawing (Projection -> Render ()) -- add a parameter (path or circle for tool) !!!!!!!!!!!!!!!!!!!!!!!!!

instance Monoid Drawing where
	mempty = Drawing $ \proj -> return ()
	Drawing f1 `mappend` Drawing f2 = Drawing $ \proj -> f1 proj >> f2 proj

instance Backend Drawing where
	bRapid dst = do
		td <- getToolDiameter
		return $ Drawing (\ proj -> do
			let (V2 x y) = proj dst	
			arc x y (td/2) 0 (2*pi)
			stroke
			)
		

	bFeed _ dst = do
		td <- getToolDiameter
		return $ Drawing (\ proj -> do
			let (V2 x y) = proj dst	
			arc x y (td/2) 0 (2*pi)
			stroke
			)

	bArc _ dir cen dst = do
		td <- getToolDiameter
		cp <- getCurrentPosition
		return $ Drawing (\ proj -> do
			let points = map proj $ arcInterpolation cp dst cen dir 1
			sequence_ [arc x y (td/2) 0 (2*pi) | V2 x y <- points]
			stroke
			)
			
	bPause = return $ Drawing (\proj -> return ())

	bProbe _ dst = do
		td <- getToolDiameter
		return $ Drawing (\ proj -> do
			let (V2 x y) = proj dst	
			setSourceRGB 0 1 0
			arc x y (td/2) 0 (2*pi)
			setSourceRGB 0 0 0
			stroke
			)

	bDefCurPos dst = do
		td <- getToolDiameter
		return $ Drawing (\ proj -> do
			let (V2 x y) = proj dst	
			setSourceRGB 1 0 0
			arc x y (td/2) 0 (2*pi)
			setSourceRGB 0 0 0
			stroke
			)

	
	bComment _ = return $ Drawing (\proj -> return ())

	bName _ op = op

getDrawingWithDefaultParameters :: Operation Drawing -> Drawing
getDrawingWithDefaultParameters = runOperationWithDefaultParams

drawOperation :: Operation Drawing -> Projection -> Render()
drawOperation op proj = f proj
	where
		Drawing f = getDrawingWithDefaultParameters op

type Canvas = Double -> Double -> Render ()

-- | Initializes a canvas, and sets the scale to maximize to have the biggest possible picture.
initCanvas :: 	Double		-- ^ w : Width of the canvas 
		-> Double	-- ^ h : Height of the canvas
		-> Double	-- ^ lx : Width of the drawing
		-> Double	-- ^ ly : Height of the drawing
		-> Render ()
initCanvas w h lx ly = do
	setSourceRGB 1 1 1
	paint
	setSourceRGB 0 0 0
	let s = if h/w < ly/lx 
			then h/ly
			else w/lx
	setMatrix (Matrix s 0 0 (-s) (w/2) (h/2))
	setLineWidth 0.1

plot :: Canvas -> Canvas -> IO ()
plot c1 c2 = do
	initGUI
	window <- windowNew

	vbox <- vBoxNew False 10
	hbox1 <- hBoxNew True 10
	hbox2 <- hBoxNew False 10

	prevBtn <- buttonNewWithLabel "Previous"
	nextBtn <- buttonNewWithLabel "Next"
	quitBtn <- buttonNewWithLabel "Quit"
	onClicked quitBtn mainQuit

	canvas1 <- drawingAreaNew
	onExpose canvas1 (\x -> do
				(w, h) <- widgetGetSize canvas1
				drw <- widgetGetDrawWindow canvas1
				renderWithDrawable drw (c1 (fromIntegral w) (fromIntegral h))
				return True)
	canvas2 <- drawingAreaNew
	onExpose canvas2 (\x -> do
				(w, h) <- widgetGetSize canvas2
				drw <- widgetGetDrawWindow canvas2
				renderWithDrawable drw (c2 (fromIntegral w) (fromIntegral h))
				return True)


	sep <- hSeparatorNew

	set window [windowTitle := "Sivi", windowDefaultWidth := 300, windowDefaultHeight := 200,
			containerBorderWidth := 15, containerChild := vbox]

	boxPackStart vbox hbox1 PackGrow 0
	boxPackStart vbox sep PackNatural 0
	boxPackStart vbox hbox2 PackNatural 0
	boxPackStart hbox1 canvas1 PackGrow 0
	boxPackStart hbox1 canvas2 PackGrow 0
	boxPackStart hbox2 prevBtn PackNatural 0
	boxPackStart hbox2 nextBtn PackNatural 0
	boxPackStart hbox2 quitBtn PackNatural 0

	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
