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
	case Map.lookup newPos world of
		Nothing -> (movedPlayer, Map.delete (pos player) $ insertCharacterToWorld movedPlayer world)
		Just _ -> error "TODO: What happens if we move onto something?"
	where
	movedPlayer = setL lensPos newPos player

updateWorld :: (Character, World) -> SDL.Event -> (Character, World)
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_UP})) =
	movePlayer player (worldPositionX ^+= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_DOWN})) =
	movePlayer player (worldPositionX ^-= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_RIGHT})) =
	movePlayer player (worldPositionY ^+= 1 $ p) world
updateWorld (player@(Character {pos = p}), world) (SDL.KeyUp (SDL.Keysym {SDL.symKey = SDL.SDLK_LEFT})) =
	movePlayer player (worldPositionY ^-= 1 $ p) world
updateWorld p _ = p

moveFromDie :: Int -> WorldPosition -> WorldPosition
moveFromDie die (WorldPosition (x, y))
	| die <  5 = WorldPosition (x+1, y)
	| die < 10 = WorldPosition (x-1, y)
	| die < 15 = WorldPosition (x, y+1)
	| die < 20 = WorldPosition (x, y-1)
	| otherwise = WorldPosition (x, y)

updatePlayerAndWorld :: Bool -> [Int] -> [SDL.Event] -> (Character, World) -> (Character, World)
updatePlayerAndWorld tick dice events (p, w)
	| tick = (p', foldl' (\world (idx, horseman) ->
			snd $ movePlayer horseman (moveFromDie (selectDie idx) (pos horseman)) world
		) w' (zip [0..] horsemen))
	| otherwise = (p', w')
	where
	selectDie idx = dice !! (idx `mod` length dice) -- TODO
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

--signalNetwork :: World -> SignalGen Ticks (Signal [SDL.Event]) -> SignalGen Ticks (Signal a)
signalNetwork initialWorld eventGen = (clockGen >>=) $ flip embed $ do
	events <- eventGen
	dice   <- effectful $ replicateM 10 (randomRIO (1::Int, 20))
	playerAndWorld <- transfer2 (initialPlayer, initialWorld) updatePlayerAndWorld dice events
	screen <- transfer initialScreen (const updateScreen) (fmap fst playerAndWorld)
	return $ composeState <$> events <*> screen <*> (fmap snd playerAndWorld)

mainLoop :: IO ()
mainLoop = void $ runMaybeT $ do
	states <- liftIO $ (signalNetwork <$> initialWorld) >>= sdlLoop 33
	mapM_ (maybe mzero (liftIO . print)) states

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
	win <- SDL.setVideoMode 800 600 16 [SDL.HWSurface,SDL.HWAccel,SDL.AnyFormat,SDL.DoubleBuf]
	mainLoop
