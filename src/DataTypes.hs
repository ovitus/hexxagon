module DataTypes where

import qualified Data.Map.Strict as Map

data Game = Game
  { hexagon :: Hexagon
  , board :: Board
  } deriving Eq

newtype Board = Board (Map.Map Position Hexagon)
  deriving Eq

data Position = Position
  { getX :: Integer
  , getY :: Integer
  } deriving (Eq, Ord, Read, Show)

data Hexagon = Red | Blue | Empty
  deriving (Eq, Read)

data Move = Move
  { initialPosition :: Position
  , finalPosition :: Position
  } deriving (Eq, Show)

type Block = (Position, Hexagon)
