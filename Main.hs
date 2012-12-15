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
import qualified Data.Text as Text

moveCharacter :: Character -> WorldPosition -> World -> Either Species (Character, World)
moveCharacter player newPos world =
	case Map.lookup newPos world of
		Nothing -> Right (movedPlayer, Map.delete (pos player) $ insertCharacterToWorld movedPlayer world)
		Just (C (Character {species = Villan})) -> Left $ species player
		Just (C x) | species player == Villan -> Left $ species x
		_ -> Right (player, world)
	where
	movedPlayer = setL lensPos newPos player

updateWorld :: Either Species (Character, World) -> SDL.Event -> Either Species (Character, World)
updateWorld (Right (player@(Character {pos = p}), world)) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_UP})) =
	moveCharacter player (worldPositionY ^+= 1 $ p) world
updateWorld (Right (player@(Character {pos = p}), world)) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_DOWN})) =
	moveCharacter player (worldPositionY ^-= 1 $ p) world
updateWorld (Right (player@(Character {pos = p}), world)) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_RIGHT})) =
	moveCharacter player (worldPositionX ^+= 1 $ p) world
updateWorld (Right (player@(Character {pos = p}), world)) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_LEFT})) =
	moveCharacter player (worldPositionX ^-= 1 $ p) world
updateWorld p _ = p

moveHorseFromDie :: Int -> WorldPosition -> WorldPosition
moveHorseFromDie die (WorldPosition (x, y))
	| die < 10  = WorldPosition (x, y+1)
	| die < 20  = WorldPosition (x, y-1)
	| otherwise = WorldPosition (x, y)

moveGoatFromDie :: Int -> WorldPosition -> WorldPosition
moveGoatFromDie die (WorldPosition (x, y))
	| die < 10  = WorldPosition (x+2, y)
	| die < 20  = WorldPosition (x-2, y)
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
colourForSpecies Goat = SDL.Color 0x99 0x99 0x00

canSee :: Character -> Character -> Bool
canSee (Character {sight = s, pos = WorldPosition (x1, y1)}) (Character {pos = WorldPosition (x2, y2)}) =
	dist <= s
	where
	dist = Distance $ floor (sqrt $ fromIntegral ((x1-x2)^(2::Int) + (y1-y2)^(2::Int)) :: Double)

isKeyUp :: SDL.Event -> Bool
isKeyUp (SDL.KeyUp {}) = True
isKeyUp _ = False

updatePlayerAndWorld :: Bool -> [Int] -> [SDL.Event] -> Either Species (Character, World) -> Either Species (Character, World)
updatePlayerAndWorld tick dice events (Right (p, w))
	| tick || not (null $ filter isKeyUp events) = updated >>= (\(p', w') ->
		let
			characters = mapMaybe fromCharacterCell (Map.elems w')
			heroes = filter ((==Hero) . species) characters
			horsemen = filter ((==Horseman) . species) characters
			goats = filter ((==Goat) . species) characters
		in
		fmap ((,) p') $
		updateWorldFor heroes (\world (_, hero) ->
			if hero `canSee` p' then
				fmap snd $ moveCharacter hero (moveToward (pos hero) (pos p')) world
			else
				return world
		) =<<
		updateWorldFor goats (\world (idx, goat) ->
			if goat `canSee` p' then
				fmap snd $ moveCharacter goat (moveToward (pos goat) (pos p')) world
			else
				fmap snd $ moveCharacter goat (moveGoatFromDie (selectDie idx) (pos goat)) world
		) =<<
		updateWorldFor horsemen (\world (idx, horseman) ->
			if horseman `canSee` p' then
				fmap snd $ moveCharacter horseman (moveToward (pos horseman) (pos p')) world
			else
				fmap snd $ moveCharacter horseman (moveHorseFromDie (selectDie idx) (pos horseman)) world
		) w')
	| otherwise = updated
	where
	updateWorldFor doodz f world = foldM f world (zip [(0::Int)..] doodz)
	selectDie idx = dice !! (idx `mod` length dice) -- TODO: this is slow
	updated = foldl' updateWorld (Right (p, w)) events
updatePlayerAndWorld _ _ _ l = l

updateScreen :: Maybe Character -> Screen -> Screen
updateScreen (Just player) = setL lensScreenPos (playerPosToScreenPos player)
updateScreen _ = id

updatePlot :: Maybe Character -> Maybe Plot -> Maybe Plot
updatePlot (Just (Character {pos = WorldPosition (_,y)})) (Just Intro) | y < 5 = Just Intro
updatePlot (Just (Character {pos = WorldPosition (_,y)})) _ | y > 5 && y < 10 = Just HeroRumour
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
			if (now - t) >= 500 then writeIORef ref now >> return True else
				return False
		) =<< input

signalNetwork :: World -> SignalGen Bool (Signal [SDL.Event]) -> SignalGen Ticks (Signal (Maybe ((Screen, Either Species World), Maybe Plot)))
signalNetwork initialWorld eventGen = (clockGen >>=) $ flip embed $ do
	events <- eventGen
	dice   <- effectful $ replicateM 10 (randomRIO (1::Int, 20))
	playerAndWorld <- transfer2 (Right (initialPlayer, initialWorld)) updatePlayerAndWorld dice events
	screen <- transfer initialScreen (const updateScreen) (fmap (hush . fmap fst) playerAndWorld)
	plot <- transfer (Just Intro) (const updatePlot) (fmap (hush . fmap fst) playerAndWorld)
	return $ composeState <$> events <*> screen <*> fmap (fmap snd) playerAndWorld <*> plot

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
plotText HeroRumour = "You have heard that there is a HERO abount.  Best be careful."

dieText :: Species -> String
dieText Hero = "The hero has defeated you, but I suppose that was inevitable."
dieText x = "You have been killed by a " ++ Text.unpack (show x) ++ ".  You can exit now."

draw :: SDL.Surface -> SDL.TTF.Font -> Screen -> Either Species World -> Maybe Plot -> MaybeT IO ()
draw win plotFont _ (Left species) _ = liftIO $ do
	black <- mapColour win (SDL.Color 0x00 0x00 0x00)
	True <- SDL.fillRect win (Just $ SDL.Rect 0 0 800 600) black
	drawWrap win plotFont (10, 10) $ dieText species
	SDL.flip win
draw win plotFont screen (Right world) plot = liftIO $ do
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

	maybe (return ()) (drawWrap win plotFont (10, 10) . plotText) plot
	rendered <- SDL.TTF.renderUTF8Blended plotFont (Text.unpack $ show $ getL (worldPositionY.lensScreenPos) screen) (SDL.Color 0xff 0xff 0xff)
	True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect 5 550 0 0)
	SDL.flip win
	where
	inLamp pos = let ScreenPosition (x,y) = worldPositionToScreenPosition screen pos in
		x >= 7 && x <= 17 && y >= 4 && y <= 14

drawWrap :: SDL.Surface -> SDL.TTF.Font -> (Int, Int) -> String -> IO ()
drawWrap win plotFont = go
	where
	go _ [] = return ()
	go (x, y) txt = do
		-- TODO: smartwrap?
		(w, h) <- SDL.TTF.utf8Size plotFont txt
		let linec = floor (fromIntegral (length txt) / (fromIntegral w / 800::Rational)) - 5
		rendered <- SDL.TTF.renderUTF8Blended plotFont (take linec txt) (SDL.Color 0xff 0xff 0xff)
		True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect x y 0 0)
		go (x, y+h) (drop linec txt)

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
