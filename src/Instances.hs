{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Instances 
  ( destructureBoard, showBoard
  , Orientation       (..)
  , BoardMode         (..)
  ) where

import  Data.Function ( on )
import qualified Data.List as List
import  Data.Ord ( Down(Down) )
import  DataTypes
import qualified Data.Map.Strict as Map

instance Show Hexagon where
  show Red   = "R"
  show Blue  = "B"
  show Empty = "E"

instance Enum Hexagon where
  succ Red  = Blue
  succ Blue = Red

instance Show Board where
  show = showBoard Edge CoordsAndHexs []

---- Show Board (Vertex Orientation) ----       ---- Show Board (Edge Orientation) ---- __    __    __    __
--  / \ / \ / \ / \ / \ / \ / \ / \ / \ / \     -- /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
-- |   |   |   |   |   |   |   |   |   |   |    -- \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--  \ / \ / \ / \ / \ / \ / \ / \ / \ / \ / \   --    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--   |   |   |   |   |   |   |   |   |   |   |  --    /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
--    \ / \ / \ / \ / \ / \ / \ / \ / \ / \ /   --    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/

showBoard :: Orientation -> BoardMode -> [Position] -> Board -> String
showBoard orientation mode pos = showOrganizedBoard orientation mode pos . organizeBoard orientation

data Orientation = Vertex | Edge
data BoardMode = Clear | OnlyHexagons | OnlyCoordinates | CoordsAndHexs | SelectiveCoords
data Side = Top | Middle | Bottom

organizeBoard :: Orientation -> Board -> [[Maybe Block]]
organizeBoard Vertex = (organizeLine <$>) . fillBoard . groupBoard . sortBoard . destructureBoard
    where
        groupBoard = List.groupBy ((==) `on` getY . fst)
        sortBoard = List.sortOn (Down . getY . fst)
        fillBoard (xH:xs@(xL:_)) = xH : gap <> fillBoard xs
            where   getLevel = fromInteger . getY . fst . head
                    gap = replicate (getLevel xH - getLevel xL - 1) []
        fillBoard list = list

organizeBoard Edge = (organizeLine <$>) . List.reverse . buildBoard 0 . destructureBoard
    where
        buildBoard _ [] = []
        buildBoard n (buildLevel n -> (levelN,rest)) = levelN : buildBoard (n+1) rest
        buildLevel n = List.partition (isAtLevel n)
        isAtLevel level = (level ==) . (\pos -> pos.getX + pos.getY) . fst

organizeLine :: [(Position, b)] -> [Maybe (Position, b)]
organizeLine = fillLine 0 . List.sortOn (getX . fst)
    where
        fillLine i (xL:xs) = if getX (fst xL) == i  then Just xL : fillLine (i+1) xs
                                                    else Nothing : fillLine (i+1) (xL:xs)
        fillLine _ [] = []


showOrganizedBoard :: Orientation -> BoardMode -> [Position] -> [[Maybe Block]] -> String
showOrganizedBoard orientation mode pos'= case orientation of
    Edge    -> concatLines . pruneBoard . (showLine <$>) . zip [1..]
    Vertex  -> concatLines              . (showLine <$>) . zip [1..]
    where
        pruneBoard b = (pruneLine (length b * 2) <$>) <$> b
        pruneLine l (c:cs) = c: drop l cs
        pruneLine _ [] = []

        showLine :: (Int, [Maybe Block]) -> [String]
        showLine (n, sortedLine) = (indentation <>) <$> (($ sortedLine) <$> [ concatMap (showHex Top)
                                                                            , concatMiddle . (showHex Middle <$>)
                                                                            , concatMap (showHex Bottom)
                                                                            ])
            where
                indentation = '\n' : replicate (hexSize*n) ' '
                showHex Top    (Just _)     = topHex
                showHex Middle (Just (p,h)) = middleHex p h
                showHex Bottom (Just (_,h)) = case (orientation,mode) of
                                                (Edge,OnlyHexagons)   -> bottomWithHex h
                                                (Edge,CoordsAndHexs)  -> bottomWithHex h
                                                (Edge,SelectiveCoords)-> bottomWithHex h
                                                _                     -> bottomHex
                showHex _      Nothing      = noHex
                concatMiddle (m1:m2:ms) = if last m1 == '|' then m1 <> concatMiddle (tail m2 : ms)
                                                            else m1 <> concatMiddle (m2 : ms)
                concatMiddle ms = concat ms

                (topHex,bottomHex,noHex) = case orientation of
                    Edge    -> (" ___    " , "\\___/   " , "        ")
                    Vertex  -> (" / \\" , " \\ /" , "    ")
                middleHex pos hex = case orientation of
                    Edge    -> case mode of
                        Clear           -> "/   \\   "
                        OnlyHexagons    -> "/   \\   "
                        OnlyCoordinates -> "/" <> show (mod pos.getX 10) <> "," <> show (mod pos.getY 10) <> "\\   "
                        CoordsAndHexs   -> "/" <> show (mod pos.getX 10) <> " " <> show (mod pos.getY 10) <> "\\   "
                        SelectiveCoords -> if   elem pos pos' 
                                           then "/" <> show (mod pos.getX 10) <> " " <> show (mod pos.getY 10) <> "\\   "
                                           else "/   \\   "
                    Vertex  -> case mode of
                        Clear           -> "|   |"
                        OnlyHexagons    -> "|   |"
                        OnlyCoordinates -> "|" <> show (mod pos.getX 10) <> "," <> show (mod pos.getY 10) <> "|"
                        CoordsAndHexs   -> "|" <> show (mod pos.getX 10) <> show hex <> show (mod pos.getY 10) <> "|"
                        SelectiveCoords -> if   elem pos pos' 
                                           then "|" <> show (mod pos.getX 10) <> show hex <> show (mod pos.getY 10) <> "|"
                                           else "|   |"
                bottomWithHex hex = "\\_" <> show hex <> "_/   "
                hexSize = case orientation of
                    Edge    -> 4
                    Vertex  -> 2

        concatLines :: [[String]] -> String
        concatLines (line1 : line2 : ls) = case orientation of
            Edge    -> head line1 <> concatLines ([ line1 !! 1 `appendLine` head line2 , line1 !! 2 `appendLine` (line2 !! 1) , line2 !! 2] : ls)
            Vertex  -> head line1 <> line1 !! 1 <> concatLines ([line1 !! 2 `appendLine` head line2 , line2 !! 1 , line2 !! 2] : ls)
            where
                appendLine (c1:cs1) (c2:cs2)    | c1 == ' ' = c2 : appendLine cs1 cs2
                                                | c2 == ' ' = c1 : appendLine cs1 cs2
                                                | otherwise = c1 : appendLine cs1 cs2
                appendLine [] cs = cs
                appendLine cs [] = cs
        concatLines ls = concat $ concat ls

destructureBoard :: Board -> [Block]
destructureBoard (Board boardMap) = Map.toList boardMap
