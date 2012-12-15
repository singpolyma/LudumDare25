module Util where

import Prelude ()
import BasicPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as SDL.TTF

import Types

playerPosToScreenPos :: Character -> WorldPosition
playerPosToScreenPos (Character {pos = WorldPosition (x, y)}) = WorldPosition (x - 12, y - 9)

insertCharacterToWorld :: Character -> World -> World
insertCharacterToWorld c = Map.insert (pos c) (C c)

colourForSpecies :: Species -> SDL.Color
colourForSpecies Villan = SDL.Color 0xcc 0xcc 0xcc
colourForSpecies Hero = SDL.Color 0x00 0x00 0xcc
colourForSpecies Horseman = SDL.Color 0x00 0xcc 0x00
colourForSpecies Goat = SDL.Color 0x99 0x99 0x00

spriteForSpecies :: Species -> Images -> SDL.Surface
spriteForSpecies Villan = notlock
spriteForSpecies Horseman = horse
spriteForSpecies Goat = goat
spriteForSpecies Hero = hero

isKeyUp :: SDL.Event -> Bool
isKeyUp (SDL.KeyUp {}) = True
isKeyUp _ = False

screenCells :: Screen -> [WorldPosition]
screenCells (Screen {screenPos = WorldPosition (x, y)}) =
	concatMap (\xoff ->
		map (\yoff -> WorldPosition (x+xoff, y+yoff)) [0..18]
	) [0..25]

mapColour :: SDL.Surface -> SDL.Color -> IO SDL.Pixel
mapColour win (SDL.Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat win) r g b

worldPositionToScreenPosition :: Screen -> WorldPosition -> ScreenPosition
worldPositionToScreenPosition (Screen {screenPos = WorldPosition (sx, sy)}) (WorldPosition (x, y)) =
	ScreenPosition (x - sx, y - sy)

screenPositionToSDL :: ScreenPosition -> Maybe SDL.Rect
screenPositionToSDL (ScreenPosition (x, y)) = Just $ SDL.Rect (x*32) (576 - (y*32)) 32 32

drawWrap :: SDL.Surface -> SDL.TTF.Font -> (Int, Int) -> String -> IO ()
drawWrap win plotFont = go
	where
	go _ [] = return ()
	go (x, y) txt = do
		(w, h) <- SDL.TTF.utf8Size plotFont txt
		let linec = floor (fromIntegral (length txt) / (fromIntegral w / 800::Rational))
		let smartWrappedLine = smartWrap linec txt
		rendered <- SDL.TTF.renderUTF8Blended plotFont smartWrappedLine (SDL.Color 0xff 0xff 0xff)
		True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect x y 0 0)
		go (x, y+h) (drop (length smartWrappedLine) txt)
	smartWrap c s
		| length s < c = s
		| otherwise =  init $ dropWhileEnd (/=' ') $ take c s
