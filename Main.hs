module Main where

import Prelude ()
import BasicPrelude
import Control.Concurrent (threadDelay)
import Data.IORef
import Foreign (finalizeForeignPtr, touchForeignPtr)
import System.Random
import Control.Error
import Data.Bool.HT

import Data.Lens.Common

import FRP.Elerea.Param
import FRP.Elerea.SDL
import SDLgfx
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Graphics.UI.SDL.TTF as SDL.TTF
import qualified Graphics.UI.SDL.Mixer as SDL.Mixer

import qualified Data.Map as Map
import qualified Data.Text as Text

import Types
import SomeMap
import Util
import Derive

initialScreen :: Screen
initialScreen = Screen {
		screenPos = playerPosToScreenPos initialPlayer
	}

moveCharacter :: Character -> WorldPosition -> World -> Either Species (Character, World)
moveCharacter player newPos world =
	case Map.lookup newPos world of
		Nothing -> Right (movedPlayer, Map.delete (pos player) $ insertCharacterToWorld movedPlayer world)
		Just (C (Character {species = Villan})) -> Left $ species player
		Just (C x) | species player == Villan -> Left $ species x
		Just _ | species player == Hero -> -- Hero can walk through anything
			Right (movedPlayer, Map.delete (pos player) $ insertCharacterToWorld movedPlayer world)
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
updateWorld pass@(Right (player@(Character {pos = p}), world)) (SDL.MouseButtonUp x y _)
	| y < 200 = moveCharacter player (worldPositionY ^+= 1 $ p) world
	| y > 400 = moveCharacter player (worldPositionY ^-= 1 $ p) world
	| x < 200 = moveCharacter player (worldPositionX ^-= 1 $ p) world
	| x > 600 = moveCharacter player (worldPositionX ^+= 1 $ p) world
	| otherwise = pass
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

canSee :: Character -> Character -> Bool
canSee (Character {sight = s, pos = WorldPosition (x1, y1)}) (Character {pos = WorldPosition (x2, y2)}) =
	dist <= s
	where
	dist = Distance $ floor (sqrt $ fromIntegral ((x1-x2)^(2::Int) + (y1-y2)^(2::Int)) :: Double)

updatePlayerAndWorld :: Bool -> [Int] -> [SDL.Event] -> Either Species (Character, World) -> Either Species (Character, World)
updatePlayerAndWorld tick dice events (Right (p, w))
	| tick || any isKeyUp events = updated >>= (\(p', w') ->
		let
			characters = mapMaybe fromCharacterCell (Map.elems w')
			followOnly = filter ((`elem`[Hero,Guard]) . species) characters
			horsemen = filter ((==Horseman) . species) characters
			goats = filter ((==Goat) . species) characters
		in
		fmap ((,) p') $
		updateWorldFor followOnly (\world (_, hero) ->
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
updatePlot (Just (Character {pos = WorldPosition (_,y)})) _ | y > 10 && y < 15 = Just Patrols
updatePlot (Just (Character {pos = WorldPosition (_,y)})) _ | y > 90 = Just HeroNear
updatePlot _ _ = Nothing

clockGen :: SignalGen Ticks (Signal Bool)
clockGen = do
	ref <- execute $ newIORef 0
	effectful1 (\now -> do
			t <- readIORef ref
			if (now - t) >= 500 then writeIORef ref now >> return True else
				return False
		) =<< input

updateMusic :: [SDL.Event] -> Bool -> Bool
updateMusic = flip $ foldl' (\state event -> case event of
		SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_m}) -> not state
		_ -> state
	)

updateKonami :: [SDL.SDLKey] -> SDL.Event -> [SDL.SDLKey]
updateKonami x@[] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_UP})) = SDL.SDLK_UP : x
updateKonami x@[SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_UP})) = SDL.SDLK_UP : x
updateKonami x@[SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_DOWN})) = SDL.SDLK_DOWN : x
updateKonami x@[SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_DOWN})) = SDL.SDLK_DOWN : x
updateKonami x@[SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_LEFT})) = SDL.SDLK_LEFT : x
updateKonami x@[SDL.SDLK_LEFT, SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_RIGHT})) = SDL.SDLK_RIGHT : x
updateKonami x@[SDL.SDLK_RIGHT, SDL.SDLK_LEFT, SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_LEFT})) = SDL.SDLK_LEFT : x
updateKonami x@[SDL.SDLK_LEFT,SDL.SDLK_RIGHT, SDL.SDLK_LEFT, SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_RIGHT})) = SDL.SDLK_RIGHT : x
updateKonami x@[SDL.SDLK_RIGHT,SDL.SDLK_LEFT,SDL.SDLK_RIGHT, SDL.SDLK_LEFT, SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_b})) = SDL.SDLK_b : x
updateKonami x@[SDL.SDLK_b,SDL.SDLK_RIGHT,SDL.SDLK_LEFT,SDL.SDLK_RIGHT, SDL.SDLK_LEFT, SDL.SDLK_DOWN, SDL.SDLK_DOWN, SDL.SDLK_UP, SDL.SDLK_UP] (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_a})) = SDL.SDLK_a : x
updateKonami _ (SDL.KeyUp _) = []
updateKonami x _ = x

composeState :: [SDL.Event] -> a -> Either Species b -> c -> d -> [SDL.SDLKey] -> Either Done (((a, b), c), d)
composeState _ _ (Left s) _ _ _ = Left $ Died s
composeState _ _ _ _ _ konami | length konami == 10 = Left Konami
composeState events screen (Right world) plot music _
	| SDL.Quit `elem` events = Left Quit
	| otherwise = Right (((screen, world), plot), music)

signalNetwork :: Bool -> World -> SignalGen Bool (Signal [SDL.Event]) -> SignalGen Ticks (Signal (Either Done (((Screen, World), Maybe Plot), Bool)))
signalNetwork musicState initialWorld eventGen = (clockGen >>=) $ flip embed $ do
	events <- eventGen
	dice   <- effectful $ replicateM 10 (randomRIO (1::Int, 20))
	playerAndWorld <- transfer2 (Right (initialPlayer, initialWorld)) updatePlayerAndWorld dice events
	music <- transfer musicState (const updateMusic) events
	screen <- transfer initialScreen (const updateScreen) (fmap (hush . fmap fst) playerAndWorld)
	plot <- transfer (Just Intro) (const updatePlot) (fmap (hush . fmap fst) playerAndWorld)
	konami <- transfer [] (const $ flip $ foldl' updateKonami) events
	return $ composeState <$> events <*> screen <*> fmap (fmap snd) playerAndWorld <*> plot <*> music <*> konami

plotText :: Plot -> String
plotText Intro = "You are the evil mastermind Notlock.  Your plan to kidnap the boy king went off great, up until it was noticed that he was gone.  Now you are trapped in the forest on the way back to your northern lair, and must evade the searchers."
plotText HeroRumour = "You have heard that there is a HERO abount.  Best be careful."
plotText Patrols = "Goatback subjects are searching back and forth, but cannot see very far.  Horsemen search only the road, but can see much further."
plotText HeroNear = "Be on your guard.  If the HERO finds you, that's the end."

dieText :: Species -> String
dieText Hero = "The hero has defeated you, but I suppose that was inevitable.  Press any key to try again."
dieText x = "You have been killed by a " ++ Text.unpack (show x) ++ ".  Press any key to try again."

canSeeBox :: Maybe SDL.Rect -> Distance -> SDL.Rect
canSeeBox (Just (SDL.Rect x y _ _)) (Distance s) =
	SDL.Rect (x - (s*32)) (y - (s*32)) (s*32*2 + 32) (s*32*2 + 32)
canSeeBox _ _ = error "Impossible canSeeBox"

draw :: (MonadIO m) => SDL.Surface -> SDL.TTF.Font -> Images -> Screen -> World -> Maybe Plot -> Bool -> m ()
draw win plotFont images@(Images {bg=bg, road=road}) screen world plot music = liftIO $ do
	if music then SDL.Mixer.resumeMusic else SDL.Mixer.pauseMusic

	-- Two passes.  One for background, and one for "stuff"
	mapM_ (\cell -> do
			let rect = screenPositionToSDL $ worldPositionToScreenPosition screen cell
			let x = case cell of
					WorldPosition (x, _) | x >= 10 && x <= 13 -> road
					_ -> bg
			True <- SDL.blitSurface x Nothing win rect
			return ()
		) (screenCells screen)

	mapM_ (\cell -> do
			let rect = screenPositionToSDL $ worldPositionToScreenPosition screen cell
			case Map.lookup cell world of
				Just (C c) | inLamp cell -> do
					True <- boxAlpha win (canSeeBox rect (sight c)) (colourForSpecies $ species c) 0x33
					True <- SDL.blitSurface (spriteForSpecies (species c) images) Nothing win rect
					return ()
				Just (I i) | inLamp cell -> do
					True <- SDL.blitSurface (spriteForItem (itemKind i) images) Nothing win rect
					return ()
				_ -> return ()
		) (screenCells screen)

	maybe (return ()) (drawWrap win plotFont (10, 10) . plotText) plot

	rendered <- SDL.TTF.renderUTF8Blended plotFont (Text.unpack $ show $ getL (worldPositionY.lensScreenPos) screen) (SDL.Color 0xff 0xff 0xff)
	True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect 5 550 0 0)

	rendered <- SDL.TTF.renderUTF8Blended plotFont "m toggles music" (SDL.Color 0xff 0xff 0xff)
	True <- SDL.blitSurface rendered Nothing win (Just $ SDL.Rect 650 550 0 0)

	SDL.flip win
	where
	inLamp pos = let ScreenPosition (x,y) = worldPositionToScreenPosition screen pos in
		x >= 7 && x <= 17 && y >= 4 && y <= 14

mainLoop :: SDL.Surface -> SDL.TTF.Font -> Images -> Bool -> IO ()
mainLoop win plotFont images musicState = eitherT handleDone (error "impossible") $ do
	states <- liftIO $ (signalNetwork musicState <$> initialWorld) >>= sdlLoop 33
	mapM_ (either throwT (uncurry $ uncurry $ uncurry $ draw win plotFont images)) states
	where
	handleDone Quit = return ()
	handleDone Konami = do
		clear
		drawWrap win plotFont (10, 10) "You found the secret way to win!  Press any key to play again."
		goPause
	handleDone (Died s) = do
		clear
		drawWrap win plotFont (10, 10) $ dieText s
		goPause
	clear = do
		black <- mapColour win (SDL.Color 0x00 0x00 0x00)
		True <- SDL.fillRect win (Just $ SDL.Rect 0 0 800 600) black
		return ()
	goPause = do
		SDL.flip win
		threadDelay 2000000 -- Ignore any events for some time, to prevent accidents
		pause
	pause = do
		e <- SDL.waitEventBlocking
		case e of
			SDL.KeyDown _ -> SDL.Mixer.pausedMusic >>= mainLoop win plotFont images . not
			SDL.Quit -> return ()
			_ -> pause

withExternalLibs :: IO () -> IO ()
withExternalLibs f = SDL.withInit [SDL.InitEverything] $ do
	True <- SDL.TTF.init
	SDL.Mixer.openAudio SDL.Mixer.defaultFrequency SDL.Mixer.AudioS16Sys 0 1024

	f

	SDL.Mixer.closeAudio
	SDL.TTF.quit
	SDL.quit

main :: IO ()
main = withExternalLibs $ do
	win <- SDL.setVideoMode 800 576 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	plotFont <- SDL.TTF.openFont "./PortLligatSans-Regular.ttf" 20
	bg <- SDL.displayFormatAlpha =<< SDL.load "./bg.png"
	road <- SDL.displayFormatAlpha =<< SDL.load "./road.png"
	notlock <- SDL.displayFormatAlpha =<< SDL.load "./notlock.png"
	horse <- SDL.displayFormatAlpha =<< SDL.load "./horseman.png"
	goat <- SDL.displayFormatAlpha =<< SDL.load "./goat.png"
	hero <- SDL.displayFormatAlpha =<< SDL.load "./hero.png"
	shrub <- SDL.displayFormatAlpha =<< SDL.load "./shrub.png"
	riff <- SDL.Mixer.loadMUS "./riff.ogg"
	SDL.Mixer.playMusic riff (-1)
	mainLoop win plotFont (Images bg road notlock horse goat hero shrub) True

	-- Need to do this so that SDL.TTF.quit will not segfault
	finalizeForeignPtr plotFont
	touchForeignPtr riff
