module Helpers (safeIndex, safeIndex2D, replace, replace2D, inRange, inRange2D, enumerate, enumerate2D, uniquePairs) where

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

inRange :: [a] -> Int -> Bool
inRange xs i = i >= 0 && i < length xs

inRange2D :: [[a]] -> (Int, Int) -> Bool
inRange2D xss (x, y) = inRange xss y && inRange (head xss) x

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D ass =
  concatMap
    ( \(y, xs) ->
        map (\(x, a) -> ((x, y), a)) xs
    )
    (enumerate (map enumerate ass))

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = concatMap (\(i, a) -> map (\b -> (a, b)) (drop (i + 1) xs)) (enumerate xs)