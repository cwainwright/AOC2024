module Day1 where
  import Parser(Parser(..), numberP, whitespaceP, parserSeparated)
  import Data.List (sort)
  
  main :: IO()
  main = do
    -- Part 1
    file_contents <- readFile "data/D1.txt"
    let parsedData = parseData file_contents
    let (leftList, rightList) = unzip parsedData
    let (sLeftList, sRightList) = (sort leftList, sort rightList)
    let sortedLists = zip sLeftList sRightList
    let differences = map (\(x, y) -> abs (x - y)) sortedLists
    let total1 = sum differences
    putStrLn $ "Part 1 Result: " ++ show total1
    -- Part 1 Complete
    -- Part 2
    let similarityScores = map (calculateSimilarity rightList) leftList
    let total2 = sum similarityScores
    putStrLn $ "Part 2 Result: " ++ show total2
    -- Part 2 Complete

  -- Part 1 Helper Functions
  parseData :: String -> [(Int, Int)]
  parseData str = 
    case parse dataParser str of
      Left _ -> []
      Right parserData -> (\(finalData, _) -> finalData) parserData
  
  dataParser :: Parser [(Int, Int)]
  dataParser = parserSeparated whitespaceP lineParser

  lineParser :: Parser (Int, Int)
  lineParser = do
    l1 <- numberP
    _ <- whitespaceP
    l2 <- numberP
    return (l1, l2)

  -- Part 2 Helper Functions
  calculateSimilarity :: [Int] -> Int -> Int
  calculateSimilarity rightList number = number * (length $ filter (\x -> x == number) rightList)