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
) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix hiding (scale)

initCanvas :: Double -> Double -> Double -> Double -> Render ()
initCanvas w h lx ly = do
	setSourceRGB 1 1 1
	paint
	setSourceRGB 0 0 0
	let s = if h/w < ly/lx 
			then h/ly
			else w/lx
	setMatrix (Matrix s 0 0 (-s) (w/2) (h/2))

draw1 :: Double -> Double -> Render ()
draw1 w h = do
	initCanvas w h 150 150
	moveTo 0 0
	lineTo 70 70
	stroke

draw2 :: Double -> Double -> Render ()
draw2 w h = do
	initCanvas w h 150 150
	moveTo (-70) 70
	lineTo 0 0
	stroke

plot :: IO ()
plot = do
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
				renderWithDrawable drw (draw1 (fromIntegral w) (fromIntegral h))
				return True)
	canvas2 <- drawingAreaNew
	onExpose canvas2 (\x -> do
				(w, h) <- widgetGetSize canvas2
				drw <- widgetGetDrawWindow canvas2
				renderWithDrawable drw (draw2 (fromIntegral w) (fromIntegral h))
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
