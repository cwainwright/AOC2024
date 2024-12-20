module Day5 where

import Parser (Parser (..), characterP, commaSeparated, numberP, whitespaceP)

main :: IO ()
main = do
  file_contents <- readFile "data/D5.txt"
  let file_lines = lines file_contents
  case (parseData file_lines) of
    Just (orderingRules, pageNumbers) -> do
      -- Part One
      let inOrder = filter (checkLineOrder orderingRules) pageNumbers
      print $ sum (map (\xs -> xs !! ((length xs) `div` 2)) inOrder)
    Nothing -> print "Nothing"

type PageOrderingRule = (Int, Int)

type PageNumbers = [Int]

pageOrderRuleP :: Parser PageOrderingRule
pageOrderRuleP = do
  num1 <- numberP
  _ <- characterP '|'
  num2 <- numberP
  return (num1, num2)

pageNumbersP :: Parser PageNumbers
pageNumbersP = do
  numbers <- commaSeparated numberP
  _ <- whitespaceP
  return numbers

parseData :: [String] -> Maybe ([PageOrderingRule], [PageNumbers])
parseData lineData = do
  let orderingRulesS = takeWhile ((/=) "") lineData
  let pageNumbersS = tail $ dropWhile ((/=) "") lineData

  orderingRulesR <- mapM (parse pageOrderRuleP) orderingRulesS
  let orderingRules = fmap (\(oR, _) -> oR) orderingRulesR

  pageNumbersR <- mapM (parse pageNumbersP) pageNumbersS
  let pageNumbers = fmap (\(oR, _) -> oR) pageNumbersR

  return (orderingRules, pageNumbers)

enumerate :: Int -> [a] -> [(Int, a)]
enumerate x xs = zip [x ..] xs

pairAfter :: [a] -> [(a, a)]
pairAfter xs = concatMap (\(i, a) -> map (\b -> (a, b)) (drop i xs)) (enumerate 1 xs)

checkLineOrder :: [PageOrderingRule] -> PageNumbers -> Bool
checkLineOrder orderingRules pageNumbers = not (any (\pair -> any (\(a, b) -> (b, a) == pair) orderingRules) (pairAfter pageNumbers))

-- evaluateOrder :: [PageOrderingRule] -> PageNumbers -> Bool
-- evaluateOrder orderingRules numbers = do
--   let pairs = pairWithNext numbers
--   True