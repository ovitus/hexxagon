module IOFxs where

import Control.Exception 
  ( IOException
  , try
  )
import Control.Monad.IO.Class 
  ( liftIO
  )
import Control.Monad.State
import Data.Char 
  ( toLower
  )
import Data.Maybe 
  ( fromJust
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
  ( checkFinalPosition
  , checkInitialPosition
  , checkWinner
  , destructureGame
  , makeMove
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
  , getTerminalSize
  , setCursorPosition
  , setSGRCode
  )
import System.Console.Haskeline
import Text.Read 
  ( readMaybe
  )
import UtilityFxs 
  ( classicBoard_S9DC3
  , getNearbyPositions
  , makeClassicBoard
  , makeEmptyClassicBoard
  )
import qualified Data.Map.Strict as Map
import qualified System.Console.ANSI as C (Color(..))

mai :: Game -> IO (Maybe Game)
mai g = runInputT defaultSettings $ checkWinnerIO g

testGI :: Game
testGI = Game Red classicBoard_S9DC3

screen :: Game -> InputT IO ()
screen (Game h b) = do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()

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
                Just g'@(Game h _) -> checkWinnerIO g'
                _ -> return Nothing
            _ -> checkWinnerIO g
          Left g -> case g of
            Just g' -> checkWinnerIO g'
            _ -> return g 
      _ -> checkWinnerIO g
    Left g -> case g of
      Just g' -> checkWinnerIO g'
      _ -> return g

saveGame :: Game -> String -> InputT IO ()
saveGame g s = do
  liftIO . writeFile (last $ words s) . show $ destructureGame g
  return ()

loadGame :: String -> InputT IO (Maybe String)
loadGame s = do
  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
  case g of
    Right g' -> return $ Just g'
    _        -> return Nothing

getInitialPosition :: Game -> InputT IO (Either (Maybe Game) (Maybe Position))
getInitialPosition g@(Game h b@(Board mph)) = do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== h) mph) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s 
      | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s
                                                                                                getInitialPosition g
      | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return . Left $ Nothing
      | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                case g' of
                                                                                                  Just g'' -> do
                                                                                                    let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                    return . Left . Just $ restructureGame g'''
                                                                                                  _ -> getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

getFinalPosition :: Game -> Position -> InputT IO (Either (Maybe Game) (Maybe (Position, Integer)))
getFinalPosition g@(Game h b) ip = do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  hexxagonTitle h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2]) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s 
      | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s
                                                                                                getFinalPosition g ip
      | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return . Left $ Nothing
      | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                case g' of
                                                                                                  Just g'' -> do
                                                                                                    let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                    return . Left . Just $ restructureGame g'''
                                                                                                  _ -> getFinalPosition g ip
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

hexxagonTitle :: Hexagon -> InputT IO ()
hexxagonTitle h = 
  if h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

hexxagonWinner :: Hexagon -> InputT IO ()
hexxagonWinner h = case h of
  Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red   , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _    -> outputStrLn $ "\n  " <> (concat $ (\(x,y) -> x <> [y]) <$> zip (cycle $ [setSGRCode [SetColor Background Dull C.Red, SetConsoleIntensity BoldIntensity], setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity]]) " HEXXAGŌN TIE! ") <> setSGRCode [Reset]


screenWinner :: Game -> InputT IO ()
screenWinner (Game h b) = do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  hexxagonWinner h
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | 'E' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> [c] <> setSGRCode [Reset]
  | otherwise = [c]

cRed :: Char -> [Char]
cRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

cBlue :: Char -> [Char]
cBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

checkWinnerIO :: Game -> InputT IO (Maybe Game)
checkWinnerIO g@(Game h b) = case checkWinner b of
  Just (Red, b') -> do screenWinner $ Game Red b'
                       s <- getInputLine "   "
                       case s of
                         Just [] -> checkWinnerIO $ Game Red b'
                         Just s 
                           | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s; checkWinnerIO g
                           | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                                     case g' of
                                                                                                                       Just g'' -> do
                                                                                                                         let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                                         checkWinnerIO $ restructureGame g'''
                                                                                                                       _ -> checkWinnerIO g
                           | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return $ Just g
                         _ -> checkWinnerIO $ Game Red b'
  Just (Blue, b') -> do screenWinner $ Game Blue b'; return . Just $ Game h b'
                        s <- getInputLine "   "
                        case s of
                          Just [] -> checkWinnerIO $ Game Blue b'
                          Just s 
                            | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s; checkWinnerIO g
                            | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                                      case g' of
                                                                                                                        Just g'' -> do
                                                                                                                          let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                                          checkWinnerIO $ restructureGame g'''
                                                                                                                        _ -> checkWinnerIO g
                            | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return $ Just g
                          _ -> checkWinnerIO $ Game Blue b'
  Just (Empty, b') -> do screenWinner $ Game Empty b'; return . Just $ Game h b'
                         s <- getInputLine "   "
                         case s of
                           Just [] -> checkWinnerIO $ Game Blue b'
                           Just s 
                             | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s; checkWinnerIO g
                             | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                                       case g' of
                                                                                                                         Just g'' -> do
                                                                                                                           let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                                           checkWinnerIO $ restructureGame g'''
                                                                                                                         _ -> checkWinnerIO g
                             | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return $ Just g
                           _ -> checkWinnerIO $ Game Blue b'
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game g
            Just s 
              | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save" -> do saveGame g s; checkWinnerIO g
              | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load" -> do g' <- loadGame s
                                                                                                        case g' of
                                                                                                          Just g'' -> do
                                                                                                            let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                            checkWinnerIO $ restructureGame g'''
                                                                                                          _ -> checkWinnerIO g
              | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit" -> return $ Just g
            _ -> game g


scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap cRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap cBlue (replicate bl ' ')) $ score b


parseList :: String -> Maybe [(Integer, Integer)]
parseList x = readMaybe x

configBoard_ = configBoard $ makeEmptyClassicBoard 9

configBoard :: Board -> IO ()
configBoard b = runInputT defaultSettings $ configBoard' b

configBoard' :: Board -> InputT IO ()
configBoard' b = do 
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  outputStrLn $ "\n   CONFIGURE BOARD\n\n\n" <> setSGRCode [Reset]
  outputStrLn . concatMap colorize $ showBoard Edge CoordsAndHexs [] b
  input <- getInputLine "   "
  case input of
    Just [] -> configBoard' b
    Just s
      | (toLower <$> (head $ words s)) == "b" || (toLower <$> (head $ words s)) == "blue"   -> configBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
      | (toLower <$> (head $ words s)) == "d" || (toLower <$> (head $ words s)) == "delete" -> configBoard' . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
      | (toLower <$> (head $ words s)) == "e" || (toLower <$> (head $ words s)) == "empty"  -> configBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
      | (toLower <$> (head $ words s)) == "r" || (toLower <$> (head $ words s)) == "red"    -> configBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
      | (toLower <$> (head $ words s)) == "s" || (toLower <$> (head $ words s)) == "save"   -> do saveGame (Game Red b) s; configBoard' b
      | (toLower <$> (head $ words s)) == "l" || (toLower <$> (head $ words s)) == "load"   -> do g' <- loadGame s
                                                                                                  case g' of
                                                                                                    Just g'' -> do
                                                                                                      let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                                                                                      configBoard' . board $ restructureGame g'''
                                                                                                    _ -> configBoard' b
      | (toLower <$> (head $ words s)) == "q" || (toLower <$> (head $ words s)) == "quit"   -> return ()
    _ -> configBoard' b
