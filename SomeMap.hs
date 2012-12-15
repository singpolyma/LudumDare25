module SomeMap (initialWorld, initialPlayer) where

import Prelude ()
import BasicPrelude hiding (guard)
import System.Random (randomRIO)
import qualified Data.Map as Map

import Types hiding (goat, hero, shrub)
import Util

hero :: Character
hero = Character {
		species = Hero,
		sight   = Distance 25,
		pos     = WorldPosition (14, 100)
	}

goat :: WorldPosition -> Character
goat p = Character {
		species = Goat,
		sight   = Distance 2,
		pos     = p
	}

goatIO :: IO Character
goatIO = do
	x <- randomRIO ( -50, 50)
	y <- randomRIO ( 5,100)
	return $ goat $ WorldPosition (x, y)

horseman :: WorldPosition -> Character
horseman p = Character {
		species = Horseman,
		sight   = Distance 5,
		pos     = p
	}

horsemanIO :: IO Character
horsemanIO = do
	x <- randomRIO (10, 13)
	y <- randomRIO (10,100)
	return $ horseman $ WorldPosition (x, y)

mkWorld :: [Character] -> World
mkWorld = Map.fromList . map (pos &&& C)

guard :: WorldPosition -> Character
guard p = Character {
		species = Guard,
		sight   = Distance 1,
		pos     = p
	}

guards :: [Character]
guards = concatMap (\x -> [
		guard (WorldPosition (x, -20)),
		guard (WorldPosition (x, 120))
	]) [-50..50]
	++
	concatMap (\y -> [
		guard (WorldPosition (-50, y)),
		guard (WorldPosition (50, y))
	]) [-20..120]

shrub :: WorldPosition -> Item
shrub p = Item {
		itemKind = Shrub,
		itemPos  = p
	}

shrubIO :: IO Item
shrubIO = do
	x <- randomRIO (-50, 50)
	y <- randomRIO (-10,120)
	return $ shrub $ WorldPosition (x, y)

shrubMap :: IO World
shrubMap = Map.fromList . map (itemPos &&& I) <$> replicateM 300 shrubIO

someMap :: IO World
someMap = (mkWorld . ((hero:guards) ++)) <$> ((++) <$> replicateM 10 horsemanIO <*> replicateM 40 goatIO)

initialPlayer :: Character
initialPlayer = Character {
		species = Villan,
		sight   = Distance 5,
		pos     = WorldPosition (12, 0)
	}

initialWorld :: IO World
initialWorld = (++) <$> (insertCharacterToWorld initialPlayer <$> someMap) <*> shrubMap
