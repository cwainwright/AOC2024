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
  let groupedAntennae = groupBy (\(_, a) (_, b) -> a == b) (sortBy sortAntennae antennae)
  let antinodes = concatMap (\(a, b) -> getAntinodes a b) $ concatMap uniquePairs groupedAntennae
  let validAntinodes = filter (inRange2D grid) antinodes
  let uniqueAntinodes = map head $ group (sort validAntinodes)
  print $ length uniqueAntinodes

getAntennae :: [[Char]] -> [Antenna]
getAntennae = filter (\(_, char) -> not (char `elem` ".#")) . enumerate2D

sortAntennae :: Antenna -> Antenna -> Ordering
sortAntennae (_, a1) (_, a2) =
  if a1 < a2
    then LT
    else
      if a1 > a2
        then GT
        else EQ

getAntinodes :: Antenna -> Antenna -> [Antinode]
getAntinodes ((ax, ay), _) ((bx, by), _) =
  let (dx, dy) = (ax - bx, ay - by)
   in (ax + dx, ay + dy) : (bx - dx, by - dy) : []
