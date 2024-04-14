module IOFxs where

import Control.Exception 
  ( IOException
  , try
  )
import Control.Monad.State
import Data.Char
  ( isDigit
  )
import Data.List
  ( inits
  )
import DataTypes 
  ( Board(..)
  , Game(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  )
import Instances 
  ( BoardMode(..)
  , Orientation(Edge)
  , showBoard
  )
import MainFxs 
  ( boardToMap
  , checkFinalPosition
  , checkInitialPosition
  , checkWinner
  , destructureGame
  , makeMove
  , match
  , modifyBoard
  , parseConfig
  , parseInput
  , restructureGame
  , score
  )
import System.Console.ANSI 
  ( ColorIntensity(..)
  , ConsoleIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , clearScreen
  , setCursorPosition
  , setSGRCode
  )
import System.Console.Haskeline
import System.IO 
  ( hFlush
  , stdout
  )
import Text.Read 
  ( readMaybe
  )
import UtilityFxs 
  ( classicBoard_S9DC3
  , getNearbyPositions
  , makeStartingBoard
  )
import qualified Data.Map.Strict as Map
import qualified System.Console.ANSI as C 
  ( Color(..)
  )

boardConfig :: Board -> InputT IO (Maybe Game)
boardConfig b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b                
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (fst <$> Map.toList (boardToMap b))) b
  boardConfigCommands b

boardConfigCommands :: Board -> InputT IO (Maybe Game)
boardConfigCommands b = do
  input <- getInputLine "   "
  case input of 
   Just [] -> boardConfig b
   Just s
     | match s "blue"   -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
     | match s "delete" -> if match (last $ words s) "all" then boardConfig . Board $ Map.fromList [] else boardConfig . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
     | match s "empty"  -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
     | match s "red"    -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
     | match s "all"    -> allM (last $ words s)
     | match s "help"   -> boardConfigHelp b
     | match s "size" && if isDigit . last . last $ words s then (elem (sParse s) [1,2,3,4]) else False -> boardConfig $ makeStartingBoard (sParseNum (read . last $ words s :: Integer)) []
     | match s "play"   -> checkWinnerIO $ Game Red b
     | match s "quit"   -> return Nothing
     | match s "load"   -> do g' <- loadGame s
                              case g' of
                                Just g'' -> do
                                  let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                  boardConfig . board $ restructureGame g'''
                                _ -> boardConfig b
     | match s "save"     -> do saveGame (Game Red b) s; boardConfig b
     | elem s $ drop 3 $ inits "reset" -> boardConfig classicBoard_S9DC3
   _ -> boardConfig b
   where                
     sParse s = read . last $ words s :: Integer
     sParseNum s = case s of
       1 -> 3
       2 -> 5
       3 -> 7
       _ -> 9
     allM s                             
       | match s "blue"   = boardConfig . Board $ foldr (\x -> Map.insert x Blue) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
       | match s "empty"  = boardConfig . Board $ foldr (\x -> Map.insert x Empty) (boardToMap b) $ fst <$> Map.toList (boardToMap b )
       | match s "red"    = boardConfig . Board $ foldr (\x -> Map.insert x Red) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
       | otherwise = boardConfig b

boardConfigHelp :: Board -> InputT IO (Maybe Game)
boardConfigHelp b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Enter 'play' or create a custom board first."
  outputStrLn ""
  outputStrLn "     size <1,2,3,4>: Adjust board size"
  outputStrLn "" 
  outputStrLn "     red <space delimited coordinates>: Make selected hexagons red"
  outputStrLn "     blue <space delimited coordinates>: Make selected hexagons blue"
  outputStrLn "     empty <space delimited coordinates>: Make selected hexagons empty"
  outputStrLn "     delete <space delimited coordinates>: Remove selected hexagons"
  outputStrLn "" 
  outputStrLn "     all red: Make all hexagons red"
  outputStrLn "     all blue: Make all hexagons blue"
  outputStrLn "     all empty: Make all hexagons empty"
  outputStrLn "     delete all: Remove all hexagons"
  outputStrLn ""
  outputStrLn "     play: Start game"
  outputStrLn "     save <filepath>: Save board layout"
  outputStrLn "     load <filepath>: Load board layout"
  outputStrLn "     reset: Restart game"
  outputStrLn "     quit: Exit game"
  outputStrLn "     help: Print this help summary page."
  outputStrLn ""
  boardConfigCommands b

boardConfigRunInputT :: IO (Maybe Game)                                        
boardConfigRunInputT = runInputT defaultSettings $ boardConfig classicBoard_S9DC3

checkWinnerIO :: Game -> InputT IO (Maybe Game)
checkWinnerIO g@(Game _ b) = case checkWinner b of
  Just (Red, b')   -> do screenWinner $ Game Red   b'
                         s <- getInputLine "   "
                         gameCommands s g
  Just (Blue, b')  -> do screenWinner $ Game Blue  b'
                         s <- getInputLine "   "
                         gameCommands s g
  Just (Empty, b') -> do screenWinner $ Game Empty b'
                         s <- getInputLine "   "
                         gameCommands s g
  _ -> do screenDefault g
          s <- getInputLine "   "
          case s of
            Just [] -> getPositions g
            _ -> gameCommands s g

colorBlue :: Char -> [Char]
colorBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

colorRed :: Char -> [Char]
colorRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | '-' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> "E" <> setSGRCode [Reset]
  | otherwise = [c]

gameCommands :: Maybe String -> Game -> InputT IO (Maybe Game)
gameCommands s g = do
  case s of
    Just [] -> checkWinnerIO g
    Just s'  -> lsqh s' g
    _       -> checkWinnerIO g
    where
      lsqh s g
        | match s "save" = do saveGame g s; checkWinnerIO g
        | match s "load" = do g' <- loadGame s
                              case g' of
                                Just g'' -> do
                                  let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                  checkWinnerIO $ restructureGame g'''
                                _ -> checkWinnerIO g
        | match s "reset" = boardConfig classicBoard_S9DC3 
        | match s "quit" = return $ Just g
        | match s "help" = gameHelp g
        | otherwise = checkWinnerIO g

gameHelp :: Game -> InputT IO (Maybe Game)
gameHelp g = do
  screenGameHelp
  s <- getInputLine "   "
  gameCommands s g

getFinalPosition :: Game -> Position -> InputT IO (Either (Int, Maybe Game) (Maybe (Position, Integer)))
getFinalPosition g@(Game h b) ip = do
  liftIO resetCursor
  titleDefault h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2])) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["save", "load", "reset", "quit", "help"] -> getPosGameCommands s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Game -> InputT IO (Either (Int, Maybe Game) (Maybe Position))
getInitialPosition g@(Game h b@(Board mph)) = do
  liftIO resetCursor
  titleDefault h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== h) mph)) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["save", "load", "reset", "quit", "help"] -> getPosGameCommands s getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

getPosGameCommands :: String -> (Game -> InputT IO (Either (Int, Maybe Game) b)) -> Game -> InputT IO (Either (Int, Maybe Game) b)
getPosGameCommands s f g
  | s == [] = f g
  | match s "save" = do saveGame g s; f g
  | match s "load" = do g' <- loadGame s
                        case g' of
                          Just g'' -> do
                            let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                            return . Left $ (0, Just $ restructureGame g''')
                          _ -> f g
  | match s "reset" = return $ Left (1, Nothing)
  | match s "quit" = return $ Left (2, Nothing)
  | match s "help" = getPosGameHelp f g
  | otherwise = return $ Left (0, Just g)

getPosGameHelp :: (Game -> InputT IO (Either (Int, Maybe Game) b)) -> Game -> InputT IO (Either (Int, Maybe Game) b)
getPosGameHelp f g = do
  screenGameHelp
  s <- getInputLine "   "
  case s of
    Just s' -> getPosGameCommands s' f g
    _       -> getPosGameHelp f g

getPositions :: Game -> InputT IO (Maybe Game)
getPositions g = do
  ip <- getInitialPosition g
  case ip of
    Right ip' -> case ip' of
      Just ip'' -> do
        fp <- getFinalPosition g ip''
        case fp of
          Right fp' -> case fp' of
            Just (fp'', d) -> do
              case makeMove g (Move ip'' fp'') d of
                Just g'@(Game _ _) -> checkWinnerIO g'
                _ -> return Nothing
            _ -> checkWinnerIO g
          Left g' -> case g' of
            (0, Just g'') -> checkWinnerIO g''
            (1, Nothing)  -> boardConfig classicBoard_S9DC3 
            _ -> return $ snd g' 
      _ -> checkWinnerIO g
    Left g' -> case g' of
      (0, Just g'') -> checkWinnerIO g''
      (1, Nothing)  -> boardConfig classicBoard_S9DC3 
      _ -> return $ snd g'

loadGame :: String -> InputT IO (Maybe String)
loadGame s = do
  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
  case g of
    Right g' -> return $ Just g'
    _        -> return Nothing

parseList :: String -> Maybe [(Integer, Integer)]
parseList x = readMaybe x

resetCursor :: IO ()
resetCursor = do
  clearScreen
  setCursorPosition 0 0
  hFlush stdout

saveGame :: Game -> String -> InputT IO ()
saveGame g s = do
  g' <- liftIO . try . writeFile (last $ words s) . show $ destructureGame g :: InputT IO (Either IOException ())
  case g' of
    Right _ -> return ()
    _ -> return ()

scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap colorRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap colorBlue (replicate bl ' ')) $ score b

screenDefault :: Game -> InputT IO ()
screenDefault (Game h b) = do
  liftIO resetCursor
  titleDefault h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

screenGameHelp :: InputT IO ()
screenGameHelp = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Enter coordinates to choose which piece to move and where."
  outputStrLn "   Pieces can move one or two spaces in either direction."
  outputStrLn "   Moving a peice one space will clone it, two will hop."
  outputStrLn "   Adjacent pieces that are your opponent will convert."
  outputStrLn "   Player with the most pieces wins."
  outputStrLn ""
  outputStrLn "     save <filepath>: Save game"
  outputStrLn "     load <filepath>: Load game"
  outputStrLn "     reset: Restart game"
  outputStrLn "     quit: Exit game"
  outputStrLn "     help: Print this help summary page."
  outputStrLn ""
  return ()

screenWinner :: Game -> InputT IO ()
screenWinner (Game h b) = do
  liftIO resetCursor
  titleWinner h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

titleDefault :: Hexagon -> InputT IO ()    
titleDefault h =                                                                   
  if   h == Red                                                                                 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

titleWinner :: Hexagon -> InputT IO ()
titleWinner h = case h of
  Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red ,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _    -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN TIE! " <> setSGRCode [Reset]
