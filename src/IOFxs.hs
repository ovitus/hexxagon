module IOFxs where

import Control.Exception 
  ( IOException
  , try
  )
import Control.Monad.State
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
  ( checkFinalPosition
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
  , boardToMap
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
  ( getNearbyPositions
  , classicBoard_S9DC3
  , makeStartingBoard
  )
import qualified Data.Map.Strict as Map
import qualified System.Console.ANSI as C (Color(..))
import Data.Char
  ( isDigit
  )
import Data.List
  ( inits
  )

cBlue :: Char -> [Char]
cBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

cRed :: Char -> [Char]
cRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

hexxHGame :: Game -> InputT IO (Maybe Game)
hexxHGame g = do
  hexxagonHelp
  s <- getInputLine "   "
  hexxGameOptions s g

posHexxHGame :: (Game -> InputT IO (Either (Int, Maybe Game) b)) -> Game -> InputT IO (Either (Int, Maybe Game) b)
posHexxHGame f g = do
  hexxagonHelp
  s <- getInputLine "   "
  case s of
    Just s' -> posHexxGameOptions s' f g
    _       -> posHexxHGame f g

hexxGameOptions :: Maybe String -> Game -> InputT IO (Maybe Game)
hexxGameOptions st ga = do
  case st of
    Just [] -> checkWinnerIO ga
    Just s  -> lsqh s ga
    _       -> checkWinnerIO ga
    where
      lsqh s g
        | match s "load" = do g' <- loadGame s
                              case g' of
                                Just g'' -> do
                                  let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                  checkWinnerIO $ restructureGame g'''
                                _ -> checkWinnerIO g
        | match s "save" = do saveGame g s; checkWinnerIO g
        | match s "help" = hexxHGame g
        | match s "quit" = return $ Just g
        | match s "reset" = createCustomBoard' classicBoard_S9DC3 
        | otherwise = checkWinnerIO g

posReset :: InputT IO (Maybe Game)
posReset = createCustomBoard' classicBoard_S9DC3 

posHexxGameOptions :: String -> (Game -> InputT IO (Either (Int, Maybe Game) b)) -> Game -> InputT IO (Either (Int, Maybe Game) b)
posHexxGameOptions s f g
  | s == [] = f g
  | match s "load" = do g' <- loadGame s
                        case g' of
                          Just g'' -> do
                            let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                            return . Left $ (0, Just $ restructureGame g''')
                          _ -> f g
  | match s "save" = do saveGame g s; f g
  | match s "help" = posHexxHGame f g
  | match s "reset" = return $ Left (1, Nothing)
  | match s "quit" = return $ Left (2, Nothing)
  | otherwise = return $ Left (0, Just g)

hexxagonHelp :: InputT IO ()
hexxagonHelp = do
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

checkWinnerIO :: Game -> InputT IO (Maybe Game)
checkWinnerIO g@(Game h b) = case checkWinner b of
  Just (Red, b')   -> do screenWinner $ Game Red   b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  Just (Blue, b')  -> do screenWinner $ Game Blue  b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  Just (Empty, b') -> do screenWinner $ Game Empty b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game g
            _ -> hexxGameOptions s g

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | 'E' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> [c] <> setSGRCode [Reset]
  | otherwise = [c]

createCustomBoardHelp :: Board -> InputT IO (Maybe Game)
createCustomBoardHelp b = do
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
  input <- getInputLine "   "
  options createCustomBoard' b input

createCustomBoard :: IO (Maybe Game)                                        
createCustomBoard = runInputT defaultSettings $ createCustomBoard' classicBoard_S9DC3
         
createCustomBoard' :: Board -> InputT IO (Maybe Game)
createCustomBoard' b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b                
  outputStrLn . concatMap colorize $ showBoard Edge CoordsAndHexs (fst <$> Map.toList (boardToMap b)) b
  input <- getInputLine "   "
  options createCustomBoard' b input

options :: (Board -> InputT IO (Maybe Game)) -> Board -> Maybe String -> InputT IO (Maybe Game)
options func b input = case input of 
  Just [] -> func b
  Just s
    | match s "blue"     -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
    | match s "delete"   -> if match (last $ words s) "all" then func . Board $ Map.fromList [] else func . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
    | match s "empty"    -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
    | match s "red"      -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
    | match s "all"      -> allM (last $ words s)
    | match s "help"     -> createCustomBoardHelp b
    | match s "size" && if isDigit . last . last $ words s then (elem (sParse s) [1,2,3,4]) else False -> func $ makeStartingBoard (sParseNum (read . last $ words s :: Integer)) []
    | match s "play"     -> checkWinnerIO $ Game Red b
    | match s "quit"     -> return Nothing
    | match s "load"     -> do g' <- loadGame s
                               case g' of
                                 Just g'' -> do
                                   let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                   func . board $ restructureGame g'''
                                 _ -> func b
    | match s "save"     -> do saveGame (Game Red b) s; func b
    | elem s $ drop 3 $ inits "reset" -> createCustomBoard' classicBoard_S9DC3
  _ -> func b
  where                
    sParse s = read . last $ words s :: Integer
    sParseNum s = case s of
      1 -> 3
      2 -> 5
      3 -> 7
      _ -> 9
    allM s                             
      | match s "blue"   = func . Board $ foldr (\x -> Map.insert x Blue) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
      | match s "empty"  = func . Board $ foldr (\x -> Map.insert x Empty) (boardToMap b) $ fst <$> Map.toList (boardToMap b )
      | match s "red"    = func . Board $ foldr (\x -> Map.insert x Red) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
      | otherwise = func b

game :: Game -> InputT IO (Maybe Game)
game g = do
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
            (1, Nothing)  -> createCustomBoard' classicBoard_S9DC3 
            _ -> return $ snd g' 
      _ -> checkWinnerIO g
    Left g' -> case g' of
      (0, Just g'') -> checkWinnerIO g''
      (1, Nothing)  -> createCustomBoard' classicBoard_S9DC3 
      _ -> return $ snd g'

getFinalPosition :: Game -> Position -> InputT IO (Either (Int, Maybe Game) (Maybe (Position, Integer)))
getFinalPosition g@(Game h b) ip = do
  liftIO resetCursor
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2]) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["load", "save", "quit", "help", "reset"] -> posHexxGameOptions s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Game -> InputT IO (Either (Int, Maybe Game) (Maybe Position))
getInitialPosition g@(Game h b@(Board mph)) = do
  liftIO resetCursor
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== h) mph) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["load", "save", "quit", "help", "reset"] -> posHexxGameOptions s getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

hexxagonTitle :: Hexagon -> InputT IO ()
hexxagonTitle h = 
  if h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

hexxagonWinner :: Hexagon -> InputT IO ()
hexxagonWinner h = case h of
  Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red ,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _    -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN TIE! " <> setSGRCode [Reset]

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
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap cRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap cBlue (replicate bl ' ')) $ score b

screen :: Game -> InputT IO ()
screen (Game h b) = do
  liftIO resetCursor
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()

screenWinner :: Game -> InputT IO ()
screenWinner (Game h b) = do
  liftIO resetCursor
  hexxagonWinner h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()
