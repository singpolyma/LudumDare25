module Types where

import BasicPrelude
import Data.Lens.Common
import qualified Graphics.UI.SDL as SDL

data Species = Villan | Hero | Horseman | Goat deriving (Eq, Show)
data Plot = Intro | HeroRumour | Patrols deriving (Eq, Show)

newtype WorldPosition = WorldPosition (Int, Int) deriving (Eq, Ord, Show)
newtype ScreenPosition = ScreenPosition (Int, Int) deriving (Eq, Ord, Show)
newtype Distance = Distance Int deriving (Eq, Ord, Show) -- In cells

data CellItem = C !Character deriving (Eq, Show)

fromCharacterCell :: CellItem -> Maybe Character
fromCharacterCell (C x) = Just x
--fromCharacterCell _ = Nothing

type World = Map WorldPosition CellItem

-- 25x18 cells
data Screen = Screen {
		screenPos :: WorldPosition -- For bottom-left cell
	} deriving (Eq, Show)

data Character = Character {
		species :: Species,
		sight   :: Distance,
		pos     :: WorldPosition
	} deriving (Eq, Show)

data Images = Images {
		bg :: SDL.Surface,
		road :: SDL.Surface,
		notlock :: SDL.Surface,
		horse :: SDL.Surface,
		goat :: SDL.Surface,
		hero :: SDL.Surface
	} deriving (Eq, Show)

worldPositionX :: Lens WorldPosition Int
worldPositionX = lens (\(WorldPosition (x, _)) -> x) (\x (WorldPosition (_, y)) -> WorldPosition (x, y))

worldPositionY :: Lens WorldPosition Int
worldPositionY = lens (\(WorldPosition (_, y)) -> y) (\y (WorldPosition (x, _)) -> WorldPosition (x, y))
