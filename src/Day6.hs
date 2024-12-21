module Day6 (main) where

import Control.Monad.State (StateT, execStateT, gets, modify, when)
import Helpers (replace2D, safeIndex2D)

type Grid = [[Char]]

data Direction = U | D | L | R deriving (Show)

data Map
  = Map
  { grid :: Grid,
    g_loc :: (Int, Int),
    g_dir :: Direction
  }
  deriving (Show)

type MapState = StateT Map IO

main :: IO ()
main = do
  file_contents <- readFile "data/D6.txt"

  let init_grid = lines file_contents
  let guard = [((x, y), cell) | (y, row) <- zip [0 ..] init_grid, (x, cell) <- zip [0 ..] row, cell `elem` "^>V<"]
  let (init_loc, init_dir_char) = head guard

  let initialState =
        Map
          { grid = init_grid,
            g_loc = init_loc,
            g_dir = charToDirection init_dir_char
          }

  -- Part One
  finalGrid1 <- fmap grid $ runMap iterMap initialState
  mapM_ putStrLn finalGrid1
  print $ countGridX finalGrid1

  -- Part Two
  finalGrid2 <- fmap grid $ runMap iterMap2 initialState
  mapM_ putStrLn finalGrid2
  print $ countGridRoute finalGrid2

getGrid :: MapState Grid
getGrid = do
  gets grid

getCoordValue :: (Int, Int) -> MapState (Maybe Char)
getCoordValue coord = safeIndex2D coord <$> getGrid

setGrid :: [[Char]] -> MapState ()
setGrid newGrid = do
  modify (\m -> m {grid = newGrid})

setCoordValue :: (Int, Int) -> Char -> MapState ()
setCoordValue coords char = do
  grid <- getGrid
  setGrid $ replace2D coords char grid

getLoc :: MapState (Int, Int)
getLoc = gets g_loc

setLoc :: (Int, Int) -> MapState ()
setLoc newLoc = modify (\m -> m {g_loc = newLoc})

getDir :: MapState Direction
getDir = gets g_dir

setDir :: Direction -> MapState ()
setDir newDir = modify (\m -> m {g_dir = newDir})

charToDirection :: Char -> Direction
charToDirection d = case d of
  '^' -> U
  'V' -> D
  '>' -> R
  '<' -> L
  _ -> undefined

nextDirection :: Direction -> Direction
nextDirection d = case d of
  U -> R
  R -> D
  D -> L
  L -> U

directionToOffset :: Direction -> (Int, Int)
directionToOffset d = case d of
  U -> (0, -1)
  D -> (0, 1)
  L -> (-1, 0)
  R -> (1, 0)

directionToRoute :: Char -> Direction -> Char
directionToRoute c d = case (c, d) of
  ('+', _) -> '+'
  ('-', U) -> '+'
  ('-', D) -> '+'
  ('-', _) -> '-'
  ('|', L) -> '+'
  ('|', R) -> '+'
  ('|', _) -> '|'
  (_, _) -> case d of
    U -> '|'
    D -> '|'
    L -> '-'
    R -> '-'

iterMap :: MapState Bool
iterMap = do
  direction <- getDir
  location@(x, y) <- getLoc
  let (x', y') = directionToOffset direction

  -- Set to visited before moving
  setCoordValue location 'X'

  let nextLoc = (x + x', y + y')
  maybeNextLoc <- safeIndex2D nextLoc <$> getGrid
  case maybeNextLoc of
    -- Off map
    Nothing -> do
      return False
    Just char -> do
      if char == '#'
        then
          -- Turn 90º
          setDir $ nextDirection direction
        else
          -- Take Step
          setLoc nextLoc
      return True

iterMap2 :: MapState Bool
iterMap2 = do
  direction <- getDir
  location@(x, y) <- getLoc
  let (x', y') = directionToOffset direction
  oldLocVal <- getCoordValue location

  let nextLoc = (x + x', y + y')
  maybeNextLoc <- safeIndex2D nextLoc <$> getGrid
  case maybeNextLoc of
    -- Off map
    Nothing -> do
      case oldLocVal of
        Nothing -> return ()
        Just locVal -> setCoordValue location (directionToRoute locVal direction)
      return False
    Just char -> case char of
      '#' -> do
        setDir $ nextDirection direction
        setCoordValue location '+'
        return True
      _ -> do
        case oldLocVal of
          Nothing -> return ()
          Just locVal -> setCoordValue location (directionToRoute locVal direction)
        setLoc nextLoc
        return True

runWhileTrue :: MapState Bool -> MapState ()
runWhileTrue action = do
  result <- action
  when result (runWhileTrue action)

runMap :: MapState Bool -> Map -> IO Map
runMap iter = execStateT (runWhileTrue iter)

countGridX :: [[Char]] -> Int
countGridX = sum . map (\row -> length $ filter (== 'X') row)

countGridRoute :: [[Char]] -> Int
countGridRoute = sum . map (\row -> length $ filter (`elem` "-|+") row)