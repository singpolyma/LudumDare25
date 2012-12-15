module Main where

import Prelude ()
import BasicPrelude
import Types
import SomeMap
import Derive
import Data.IORef
import Foreign (finalizeForeignPtr)
import System.Random
import Control.Error
import Data.Bool.HT

import Data.Lens.Common

import FRP.Elerea.Param
import FRP.Elerea.SDL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as SDL.TTF

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

moveToward :: WorldPosition -> WorldPosition -> WorldPosition
moveToward pos@(WorldPosition (x1, y1)) (WorldPosition (x2, y2)) =
	select pos [
		(x1 > x2, WorldPosition (x1-1, y1)),
		(y1 > y2, WorldPosition (x1, y1-1)),
		(x1 < x2, WorldPosition (x1+1, y1)),
		(y1 < y2, WorldPosition (x1, y1+2))
	]

colourForSpecies :: Species -> SDL.Color
colourForSpecies Villan = SDL.Color 0xcc 0xcc 0xcc
colourForSpecies Hero = SDL.Color 0x00 0x00 0xcc
colourForSpecies Horseman = SDL.Color 0x00 0xcc 0x00

canSee :: Character -> Character -> Bool
canSee (Character {sight = s, pos = WorldPosition (x1, y1)}) (Character {pos = WorldPosition (x2, y2)}) =
	dist <= s
	where
	dist = Distance $ floor (sqrt $ fromIntegral ((x1-x2)^(2::Int) + (y1-y2)^(2::Int)) :: Double)

updatePlayerAndWorld :: Bool -> [Int] -> [SDL.Event] -> (Character, World) -> (Character, World)
updatePlayerAndWorld tick dice events (p, w)
	| tick = (p', foldl' (\world (idx, horseman) ->
			if horseman `canSee` p' then
				snd $ movePlayer horseman (moveToward (pos horseman) (pos p')) world
			else
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

updatePlot :: Character -> Maybe Plot -> Maybe Plot
updatePlot (Character {pos = WorldPosition (_,y)}) (Just Intro) | y < 5 = Just Intro
updatePlot _ _ = Nothing

composeState :: [SDL.Event] -> t1 -> t2 -> t -> Maybe ((t1, t2), t)
composeState events screen world plot
	| SDL.Quit `elem` events = Nothing
	| otherwise = Just ((screen, world), plot)

clockGen :: SignalGen Ticks (Signal Bool)
clockGen = do
	ref <- execute $ newIORef 0
	effectful1 (\now -> do
			t <- readIORef ref
			if (now - t) >= 1000 then writeIORef ref now >> return True else
				return False
		) =<< input

signalNetwork :: World -> SignalGen Bool (Signal [SDL.Event]) -> SignalGen Ticks (Signal (Maybe ((Screen, World), Maybe Plot)))
signalNetwork initialWorld eventGen = (clockGen >>=) $ flip embed $ do
	events <- eventGen
	dice   <- effectful $ replicateM 10 (randomRIO (1::Int, 20))
	playerAndWorld <- transfer2 (initialPlayer, initialWorld) updatePlayerAndWorld dice events
	screen <- transfer initialScreen (const updateScreen) (fmap fst playerAndWorld)
	plot <- transfer (Just Intro) (const updatePlot) (fmap fst playerAndWorld)
	return $ composeState <$> events <*> screen <*> fmap snd playerAndWorld <*> plot

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

plotText :: Plot -> String
plotText Intro = "You are the evil mastermind Notlock.  Your plan to kidnap the boy king went off great, up until it was notice he was gone.  Now you are trapped in the forest on the way back to your lair, and must evade the searchers."

draw :: SDL.Surface -> SDL.TTF.Font -> Screen -> World -> Maybe Plot -> MaybeT IO ()
draw win plotFont screen world plot = liftIO $ do
	roadColour <- mapColour win (SDL.Color 0xcc 0x00 0x00)
	lampColour <- mapColour win (SDL.Color 0xcc 0xcc 0x00)
	mapM_ (\cell ->
			case Map.lookup cell world of
				Just (C c) | inLamp cell -> do
					colour <- mapColour win $ colourForSpecies (species c)
					True <- SDL.fillRect win (screenPositionToSDL $ worldPositionToScreenPosition screen (pos c)) colour
					return ()
				_ -> do
					colour <- case cell of
						_ | inLamp cell -> return lampColour
						WorldPosition (x, _) | x >= 10 && x <= 13 -> return roadColour
						_ -> mapColour win $ SDL.Color 0x00 0x00 0x00
					True <- SDL.fillRect win (screenPositionToSDL $ worldPositionToScreenPosition screen cell) colour
					return ()
		) (screenCells screen)

	maybe (return ()) (drawPlot (10, 10) . plotText) plot
	SDL.flip win
	where
	inLamp pos = let ScreenPosition (x,y) = worldPositionToScreenPosition screen pos in
		x >= 7 && x <= 17 && y >= 4 && y <= 14
	drawPlot _ [] = return ()
	drawPlot (x, y) txt = do
		-- TODO: smartwrap?
		(w, h) <- SDL.TTF.utf8Size plotFont txt
		let linec = floor (fromIntegral (length txt) / (fromIntegral w / 800::Rational)) - 5
		rendered <- SDL.TTF.renderUTF8Blended plotFont (take linec txt) (SDL.Color 0xff 0xff 0xff)
		True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect x y 0 0)
		drawPlot (x, y+h) (drop linec txt)

mainLoop :: SDL.Surface -> SDL.TTF.Font -> IO ()
mainLoop win plotFont = void $ runMaybeT $ do
	states <- liftIO $ (signalNetwork <$> initialWorld) >>= sdlLoop 33
	mapM_ (maybe mzero (uncurry $ uncurry $ draw win plotFont)) states

withExternalLibs :: IO () -> IO ()
withExternalLibs f = SDL.withInit [SDL.InitEverything] $ do
	True <- SDL.TTF.init

	f

	SDL.TTF.quit
	SDL.quit

main :: IO ()
main = withExternalLibs $ do
	win <- SDL.setVideoMode 800 576 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	plotFont <- SDL.TTF.openFont "./PortLligatSans-Regular.ttf" 20
	mainLoop win plotFont

	-- Need to do this so that SDL.TTF.quit will not segfault
	finalizeForeignPtr plotFont
