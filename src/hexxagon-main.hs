module Main where

import IOFxs 
  ( createCustomBoard
  )
import DataTypes
  ( Game(..)
  )

main :: IO (Maybe Game)
main = createCustomBoard
