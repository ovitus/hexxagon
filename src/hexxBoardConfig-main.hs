module Main where

import IOFxs 
  ( configBoard
  )
import UtilityFxs 
  ( makeEmptyClassicBoard
  )

main :: IO ()
main = configBoard $ makeEmptyClassicBoard 9
