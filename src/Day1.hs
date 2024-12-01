module Day1 where
  import Parser(Parser(..), numberP, whitespaceP, parserSeparated)
  import Data.List (sort)
  
  main :: IO()
  main = do
    file_contents <- readFile "data/D1.txt"
    let parsedData = parseData file_contents
    let (list1, list2) = unzip parsedData
    let (sList1, sList2) = (sort list1, sort list2)
    let sortedLists = zip sList1 sList2
    let differences = map (\(x, y) -> abs (x - y)) sortedLists
    let total = sum differences
    putStrLn $ show total

  
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