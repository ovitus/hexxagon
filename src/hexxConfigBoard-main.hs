module Main where

import DataTypes 
  ( Game(..)
  , Hexagon(..)
  )
import IOFxs 
  ( checkWinnerIO
  )
import System.Console.Haskeline
import UtilityFxs 
  ( classicBoard_S9DC3
  )

main :: IO (Maybe Game)
main = runInputT defaultSettings $ checkWinnerIO $ Game Red classicBoard_S9DC3
