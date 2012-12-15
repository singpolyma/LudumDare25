module SomeMap where

import Prelude ()
import BasicPrelude
import Types
import System.Random (randomRIO)
import qualified Data.Map as Map

hero :: Character
hero = Character {
		species = Hero,
		sight   = Distance 4,
		pos     = WorldPosition (20, 15)
	}

horseman :: WorldPosition -> Character
horseman p = Character {
		species = Horseman,
		sight   = Distance 2,
		pos     = p
	}

horsemanIO :: IO Character
horsemanIO = do
	x <- randomRIO (10, 13)
	y <- randomRIO ( 5,180)
	return $ horseman $ WorldPosition (x, y)

mkWorld :: [Character] -> World
mkWorld = Map.fromList . map (pos &&& C)

someMap :: IO World
someMap = fmap (mkWorld . (hero:)) (replicateM 10 horsemanIO)

initialPlayer :: Character
initialPlayer = Character {
		species = Villan,
		sight   = Distance 5,
		pos     = WorldPosition (12, 0)
	}

initialWorld :: IO World
initialWorld = insertCharacterToWorld initialPlayer <$> someMap

initialScreen :: Screen
initialScreen = Screen {
		screenPos = playerPosToScreenPos initialPlayer
	}

playerPosToScreenPos :: Character -> WorldPosition
playerPosToScreenPos (Character {pos = WorldPosition (x, y)}) = WorldPosition (x - 12, y - 9)

insertCharacterToWorld :: Character -> World -> World
insertCharacterToWorld c = Map.insert (pos c) (C c)
