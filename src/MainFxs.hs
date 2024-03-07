module MainFxs (checkFinalPosition, checkInitialPosition, checkWinner, destructureGame, fillBoard, makeMove, modifyBoard, noMoves, parseConfig, parseInput, restructureGame, score) where

import Data.Char (digitToInt, isDigit)
import qualified Data.Map.Strict as Map
import DataTypes (Board(..),Game(..),Hexagon(..),Move(..),Position(..), Block)
import UtilityFxs (distance, getNearbyPositions)

parseInput :: String -> Maybe Position
parseInput s = case length $ words s of 
  1 -> case s of
       (x:y:[]) -> 
         if   all isDigit [x] && all isDigit [y] 
         then Just $ Position (read [x]) (read [y]) 
         else Nothing
       _ -> Nothing
  2 -> case words s of
       [x,y] -> 
         if   all isDigit x && all isDigit y 
         then Just $ Position (read x) (read y) 
         else Nothing
       _ -> Nothing
  _ -> Nothing

parseConfig :: [String] -> [(Integer,Integer)]
parseConfig = fmap (\s -> (toInteger . digitToInt $ head s, toInteger . digitToInt $ last s)) <$> filter (\s -> all (== True) (fmap isDigit s) && length s == 2)

checkInitialPosition :: Game -> Position -> Maybe Position
checkInitialPosition (Game h b@(Board mph)) ip = case Map.lookup ip mph of
  Just h' -> if   h == h' && length (concat $ getNearbyPositions b ip Empty <$> [1,2]) > 0
             then Just ip
             else Nothing
  _ -> Nothing

checkFinalPosition :: Board -> Move -> Maybe (Position, Integer)
checkFinalPosition (Board mph) m@(Move _ fp) = case Map.lookup fp mph of
  Just Empty -> case distance m of
    1 -> Just (fp, 1)
    2 -> Just (fp, 2)         
    _ -> Nothing
  _ -> Nothing

makeMove :: Game -> Move -> Integer -> Maybe Game
makeMove (Game h b@(Board mph)) (Move ip fp) d = case d of
  1 -> Just . Game (succ h) $
       Board $ foldr 
       (\p -> Map.insert p h) 
       (Map.insert fp h mph)
       (getNearbyPositions b fp (succ h) 1)
  2 -> Just . Game (succ h) $
       Board $ foldr 
       (\p -> Map.insert p h) 
       (Map.insert fp h $ Map.insert ip Empty mph)
       (getNearbyPositions b fp (succ h) 1)
  _ -> Nothing

checkWinner :: Board -> Maybe (Hexagon, Board)
checkWinner b@(Board mph)
  | not . elem Blue $ Map.elems mph = Just (Red, b)
  | not . elem Red  $ Map.elems mph = Just (Blue, b)
  | noMoves Red b = case (score $ completeBoard Blue b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Blue b)
                | r < bl -> Just (Blue, completeBoard Blue b)
                | otherwise -> Just (Empty, completeBoard Blue b)
  | noMoves Blue b = case (score $ completeBoard Red b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Red b)
                | r < bl -> Just (Blue, completeBoard Red b)
                | otherwise -> Just (Empty, completeBoard Red b)
  | otherwise = Nothing

noMoves :: Hexagon -> Board -> Bool
noMoves h b@(Board mph) = null . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (Map.keys $ Map.filter (== h) mph)

completeBoard :: Hexagon -> Board -> Board
completeBoard h b = case noMoves h b of  
  False -> completeBoard h $ fillBoard h b 
  _     -> b

fillBoard :: Hexagon -> Board -> Board
fillBoard h b@(Board mph) = Board . foldr (\x -> Map.insert x h) mph . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (Map.keys $ Map.filter (== h) mph)

score :: Board -> (Int, Int)
score (Board b) = (length . Map.toList $ Map.filter (== Red) b, length . Map.toList $ Map.filter (== Blue) b)

destructureGame :: Game -> (Char, [((Integer, Integer), Char)])
destructureGame (Game h (Board mph)) = (hexToChar h, ((\(p,h) -> ((getX p, getY p), hexToChar h)) <$> Map.toList mph))
  where
    hexToChar h = case h of 
      Red   -> 'r'
      Blue  -> 'b'
      Empty -> 'e'

restructureGame :: (Char, [((Integer, Integer), Char)]) -> Game
restructureGame (c, iic) = Game (charToHex c) (Board . Map.fromList $ (\((x,y), c) -> (Position x y, charToHex c)) <$> iic)
  where
    charToHex c = case c of
      'r' -> Red
      'b' -> Blue
      'e' -> Empty

modifyBoard :: Board -> [Either Position Block] -> Board
modifyBoard board mods = Board $ foldr modifyBoard' b mods
   where                                                                           
     (Board b) = board
     modifyBoard' (Left   position)   = Map.delete position                                                      
     modifyBoard' (Right (pos,hex))   = Map.insert pos hex 

