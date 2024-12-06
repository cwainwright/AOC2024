module Day4 where

import Data.List (findIndices)

main :: IO ()
main = do
  fileA_contents <- readFile "data/D4.txt"
  let grid = lines fileA_contents

  -- Part One
  let gridCoords = coordGenerator grid
      xmasCount = sum $ map (\coord -> (checkForXmas grid coord)) gridCoords
  print xmasCount

  -- Part Two
  let gridHeight = length grid
      gridWidth = length (grid !! 0)
      -- Create a 1-element border around search area (to avoid OOB errors)
      boundedCoords = coordBoundedGenerator 1 (gridWidth - 1) 1 (gridHeight - 1)
      countXmas = length $ filter (\coord -> (checkForMASX grid coord)) boundedCoords
  print countXmas

-- let result = (show $ map (getArea grid) countXmas) ++ "\n"
-- writeFile "newFile.txt" result

type Grid = [[Char]]

type Coordinates = (Int, Int)

type Offset = Coordinates

xmas'x'Coordinates :: Grid -> [Coordinates]
xmas'x'Coordinates grid =
  concatMap
    ( \(y, row) ->
        map
          (\x -> (x, y))
          (findIndices ((==) 'X') row)
    )
    (zip ([0 ..] :: [Int]) grid)

coordGenerator :: Grid -> [Coordinates]
coordGenerator grid =
  [ (x, y)
    | x <- [0 .. length (grid !! 0) - 1],
      y <- [0 .. length grid - 1]
  ]

offsetGenerator :: [Offset]
offsetGenerator = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

checkStr :: Grid -> Coordinates -> Offset -> String -> Bool
checkStr _ _ _ [] = True
-- Read Character in Offset and Compare to next Character
checkStr grid (x, y) (x', y') (s : ss) =
  -- if
  if (0 <= x && x < length (grid !! 0)) && (0 <= y && y < length grid)
    then
      getCoordValue grid (x, y) == s
        -- Recursively call checkStr with shortened string and applied offset
        && (checkStr grid (x + x', y + y') (x', y') ss)
    else
      False

checkForXmas :: Grid -> Coordinates -> Int
checkForXmas grid coordinates =
  let offsets = offsetGenerator
      validWords = map (\offset -> checkStr grid coordinates offset "XMAS") offsets
   in length $ filter id validWords

-- Generate coordinates from lower (inclusive) to upper (non-inclusive)
coordBoundedGenerator :: Int -> Int -> Int -> Int -> [Coordinates]
coordBoundedGenerator lowerX upperX lowerY upperY =
  [ (x, y)
    | x <- [lowerX .. upperX - 1],
      y <- [lowerY .. upperY - 1]
  ]

getCoordValue :: Grid -> Coordinates -> Char
getCoordValue grid (x, y) = (grid !! y) !! x

checkForMASX :: Grid -> Coordinates -> Bool
checkForMASX grid coord@(x, y) =
  let offsets = [(x', y') | y' <- [-1, 1], x' <- [-1, 1]] :: [Offset]
      diagonals = map (\(x', y') -> getCoordValue grid (x + x', y + y')) offsets
      ms = filter (== 'M') diagonals
      ss = filter (== 'S') diagonals
   in -- Check for 'A' in centre
      (getCoordValue grid coord == 'A')
        -- Check for 2 'M's on diagonal
        && length ms == 2
        -- Check for 2 'S's on diagonal
        && length ss == 2
        -- Check Diagonal is not equal
        && head diagonals /= head (reverse diagonals)

-- getArea :: Grid -> Coordinates -> [String]
-- getArea grid (x, y) =
--   [ [coordValue (x - 1, y - 1), coordValue (x, y - 1), coordValue (x + 1, y - 1)],
--     [coordValue (x - 1, y), coordValue (x, y), coordValue (x + 1, y)],
--     [coordValue (x - 1, y + 1), coordValue (x, y + 1), coordValue (x + 1, y + 1)]
--   ]
--   where
--     coordValue :: Coordinates -> Char
--     coordValue = getCoordValue grid