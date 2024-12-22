module Day8 where

import Data.List (group, groupBy, sort, sortBy)
import Helpers (enumerate2D, inRange2D, uniquePairs)

type Antenna = ((Int, Int), Char)

type Antinode = (Int, Int)

main :: IO ()
main = do
  file_contents <- readFile "data/D8.txt"
  let grid = lines file_contents

  -- Part One
  let antennae = getAntennae grid
      groupedAntennae = groupBy (\(_, a) (_, b) -> a == b) (sortBy sortAntennae antennae)
      antinodes1 = concatMap getAntinodes $ concatMap uniquePairs groupedAntennae
      validAntinodes1 = filter (inRange2D grid) antinodes1
      uniqueAntinodes1 = map head $ group (sort validAntinodes1)
  print $ length uniqueAntinodes1

  -- Part Two
  let antinodes2 = concatMap (getAntinodesEx grid) $ concatMap uniquePairs groupedAntennae
      uniqueAntinodes2 = map head $ group (sort antinodes2)
  print $ length uniqueAntinodes2

getAntennae :: [[Char]] -> [Antenna]
getAntennae = filter (\(_, char) -> char `notElem` ".#") . enumerate2D

sortAntennae :: Antenna -> Antenna -> Ordering
sortAntennae (_, a1) (_, a2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | otherwise = EQ

getAntinodes :: (Antenna, Antenna) -> [Antinode]
getAntinodes (((ax, ay), _), ((bx, by), _)) =
  let (dx, dy) = (ax - bx, ay - by)
   in [(ax + dx, ay + dy), (bx - dx, by - dy)]

getAntinodesEx :: [[Char]] -> (Antenna, Antenna) -> [Antinode]
getAntinodesEx grid (((ax, ay), _), ((bx, by), _)) =
  let (dx, dy) = (ax - bx, ay - by)
   in takeWhile (inRange2D grid) [(ax + dx * mul, ay + dy * mul) | mul <- [0 ..]]
        ++ takeWhile (inRange2D grid) [(bx - dx * mul, by - dy * mul) | mul <- [0 ..]]