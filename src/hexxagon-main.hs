module Main where

import IOFxs 
  ( boardConfigRunInputT
  )
import DataTypes
  ( Game(..)
  )

main :: IO (Maybe Game)
main = boardConfigRunInputT
