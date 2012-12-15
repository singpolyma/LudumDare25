module Main where

import Prelude ()
import BasicPrelude
import Types
import SomeMap
import Derive
import Data.IORef
import System.Random
import Control.Error

import Data.Lens.Common

import FRP.Elerea.Param
import FRP.Elerea.SDL
import qualified Graphics.UI.SDL as SDL

import qualified Data.Map as Map

movePlayer :: Character -> WorldPosition -> World -> (Character, World)
movePlayer player newPos world =
	(movedPlayer, insertCharacterToWorld movedPlayer $ Map.delete (pos player) world)
	{-
	case Map.lookup newPos world of
		Nothing -> (movedPlayer, Map.delete (pos player) $ insertCharacterToWorld movedPlayer world)
		Just _ -> error "TODO: What happens if we move onto something?"
	-}
	where
	movedPlayer = setL lensPos newPos player

updateWorld :: (Character, World) -> SDL.Event -> (Character, World)
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_UP})) =
	movePlayer player (worldPositionY ^+= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_DOWN})) =
	movePlayer player (worldPositionY ^-= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_RIGHT})) =
	movePlayer player (worldPositionX ^+= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_LEFT})) =
	movePlayer player (worldPositionX ^-= 1 $ p) world
updateWorld p _ = p

moveFromDie :: Int -> WorldPosition -> WorldPosition
moveFromDie die (WorldPosition (x, y))
	| die < 10  = WorldPosition (x, y+1)
	| die < 20  = WorldPosition (x, y-1)
	| otherwise = WorldPosition (x, y)

colourForSpecies :: Species -> SDL.Color
colourForSpecies Villan = SDL.Color 0xcc 0xcc 0xcc
colourForSpecies Hero = SDL.Color 0x00 0x00 0xcc
colourForSpecies Horseman = SDL.Color 0x00 0xcc 0x00

updatePlayerAndWorld :: Bool -> [Int] -> [SDL.Event] -> (Character, World) -> (Character, World)
updatePlayerAndWorld tick dice events (p, w)
	| tick = (p', foldl' (\world (idx, horseman) ->
			snd $ movePlayer horseman (moveFromDie (selectDie idx) (pos horseman)) world
		) w' (zip [0..] horsemen))
	| otherwise = (p', w')
	where
	selectDie idx = dice !! (idx `mod` length dice) -- TODO: this is slow
	horsemen = filter ((==Horseman) . species) characters
	characters = mapMaybe fromCharacterCell (Map.elems w')
	(p', w') = foldl' updateWorld (p, w) events

updateScreen :: Character -> Screen -> Screen
updateScreen player = setL lensScreenPos (playerPosToScreenPos player)

composeState events screen world
	| SDL.Quit `elem` events = Nothing
	| otherwise = Just (screen, world)

clockGen :: SignalGen Ticks (Signal Bool)
clockGen = do
	ref <- execute $ newIORef 0
	effectful1 (\now -> do
			t <- readIORef ref
			if (now - t) >= 1000 then writeIORef ref now >> return True else
				return False
		) =<< input

signalNetwork :: World -> SignalGen Bool (Signal [SDL.Event]) -> SignalGen Ticks (Signal (Maybe (Screen, World)))
signalNetwork initialWorld eventGen = (clockGen >>=) $ flip embed $ do
	events <- eventGen
	dice   <- effectful $ replicateM 10 (randomRIO (1::Int, 20))
	playerAndWorld <- transfer2 (initialPlayer, initialWorld) updatePlayerAndWorld dice events
	screen <- transfer initialScreen (const updateScreen) (fmap fst playerAndWorld)
	return $ composeState <$> events <*> screen <*> (fmap snd playerAndWorld)

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

draw :: SDL.Surface -> Screen -> World -> MaybeT IO ()
draw win screen world = liftIO $ do
	roadColour <- mapColour win (SDL.Color 0xcc 0x00 0x00)
	let Just (SDL.Rect {SDL.rectX = roadX}) = screenPositionToSDL $ worldPositionToScreenPosition screen (WorldPosition (10, 0))
	True <- SDL.fillRect win (Just $ SDL.Rect roadX 0 128 600) roadColour
	mapM_ (\cell ->
			case Map.lookup cell world of
				Just (C c) -> do
					colour <- mapColour win $ colourForSpecies (species c)
					True <- SDL.fillRect win (screenPositionToSDL $ worldPositionToScreenPosition screen (pos c)) colour
					return ()
				_ -> do
					colour <- case cell of
						WorldPosition (x, _) | x >= 10 && x <= 13 -> return roadColour
						_ -> mapColour win $ SDL.Color 0x00 0x00 0x00
					True <- SDL.fillRect win (screenPositionToSDL $ worldPositionToScreenPosition screen cell) colour
					return ()
		) (screenCells screen)
	SDL.flip win

mainLoop :: SDL.Surface -> IO ()
mainLoop win = void $ runMaybeT $ do
	states <- liftIO $ (signalNetwork <$> initialWorld) >>= sdlLoop 33
	mapM_ (maybe mzero (uncurry $ draw win)) states

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
	win <- SDL.setVideoMode 800 576 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	mainLoop win
