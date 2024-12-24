{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (dropWhileEnd, findIndex, findIndices)
import Helpers (replace)

data DiskSector = Block Int | Free

type Disk = [DiskSector]

instance Show DiskSector where
  show :: DiskSector -> String
  show (Block bId) = show bId
  show (Free) = "."

main :: IO ()
main = do
  file_contents <- readFile "data/D9.txt"
  let disk = toDisk 0 file_contents

  defragged <- replaceNext disk
  putStrLn $ concatMap show defragged
  let checksum = checkSumDisk 0 defragged
  print checksum

toDisk :: Int -> [Char] -> Disk
toDisk _ [] = []
toDisk bId [b] = replicate (read [b]) (Block (bId))
toDisk bId (b : f : dRem) =
  replicate (read [b]) (Block (bId))
    ++ replicate (read [f]) (Free)
    ++ toDisk (bId + 1) dRem

hasFreeTail :: [DiskSector] -> Bool
hasFreeTail = all isFree . dropWhile (not . isFree)

isFree :: DiskSector -> Bool
isFree = \case Free -> True; (Block _) -> False

isBlock :: DiskSector -> Bool
isBlock = \case (Block _) -> True; Free -> False

trimFrees :: Disk -> Disk
trimFrees = dropWhileEnd isFree

nextFree :: Disk -> Maybe Int
nextFree = findIndex isFree

lastBlock :: Disk -> Int
lastBlock = last . (findIndices isBlock)

replaceNext :: Disk -> IO Disk
replaceNext disk =
  if hasFreeTail disk
    then return disk
    else do
      let blockIndex = lastBlock disk
          mFreeIndex = nextFree disk
      print blockIndex
      case mFreeIndex of
        Nothing -> return disk
        Just freeIndex -> do
          let replacedFree = replace disk freeIndex (disk !! blockIndex)
              replacedBlock = replace replacedFree blockIndex Free
          replaceNext replacedBlock

checkSumDisk :: Int -> Disk -> Int
checkSumDisk _ [] = 0
checkSumDisk index (x : xs) = case x of
  Free -> 0
  Block bId -> index * bId + checkSumDisk (index + 1) xs