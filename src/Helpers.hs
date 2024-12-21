module Helpers (safeIndex, safeIndex2D, replace, replace2D, inRange, inRange2D) where

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

safeIndex2D :: (Int, Int) -> [[a]] -> Maybe a
safeIndex2D (x, y) xss = (safeIndex y xss) >>= (safeIndex x)

replace :: Int -> a -> [a] -> [a]
replace i a xs =
  let before = take i xs
      after = drop (i + 1) xs
   in before ++ [a] ++ after

replace2D :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2D (x, y) a xss =
  let xs = xss !! y
      newXS = replace x a xs
   in replace y newXS xss

inRange :: Int -> [a] -> Bool
inRange i xs = i >= 0 && i < length xs

inRange2D :: (Int, Int) -> [[a]] -> Bool
inRange2D (x, y) xss = inRange y xss && inRange x (head xss)