module Day2(main) where
    type Safe = Bool
    data Direction = Up | Down deriving (Eq, Show)

    main :: IO ()
    main = do
        -- Load File, Parse Contents
        file_contents <- readFile "data/D2.txt"
        let file_lines :: [[Int]] = map (\l -> map read (words l)) $ lines file_contents

        -- Part One
        let part_1_safe_lines = filter problem file_lines
        print $ length part_1_safe_lines

        -- Part Two
        let part_2_safe_lines = filter problem_dampner file_lines
        print $ length part_2_safe_lines

    line_pairs :: [Int] -> [(Int, Int)]
    line_pairs line = zip line $ tail line

    problem :: [Int] -> Safe
    problem = line_safety.  line_pairs

    line_safety :: [(Int, Int)] -> Safe
    line_safety pairs = let 
        safe_direction_pairs = map (\(a, b) -> (pair_differences a b, pair_direction a b)) pairs
        in 
        all (\(safety, _) -> safety) safe_direction_pairs -- Check Safe DistancesB
        && (
            (all (\(_, direction) -> direction == Up) safe_direction_pairs) -- If all directions are Up
            || (all (\(_, direction) -> direction == Down) safe_direction_pairs)) -- If all directions are Down

    pair_differences :: Int -> Int -> Safe
    pair_differences a b = let
             diff = abs (a - b)
             in diff >= 1 && diff <= 3

    pair_direction :: Int -> Int -> Direction
    pair_direction a b = if a < b then Up else Down

    line_variations :: [Int] -> [[Int]]
    line_variations line = map (
        \i -> let (before, after) = splitAt i line
            in take (i-1) before ++ after) [1..length line]

    problem_dampner :: [Int] -> Safe
    problem_dampner file_line = do
        let variations = map line_pairs (line_variations file_line)
        let variations_safety = zip variations (map line_safety variations)
        let safe_variations = filter (\(_, safe) -> safe) variations_safety
        length safe_variations >= 1
        