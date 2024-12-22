module Day7 where

import Control.Applicative (some)
import Parser (Parser (parse), characterP, numberP, whitespaceP)
import Prelude hiding ((||))

type YXS = (Int, [Int])

(||) :: Int -> Int -> Int
(||) a b = read $ (show a) ++ (show b)

main :: IO ()
main = do
  file_contents <- readFile "data/D7.txt"
  let file_lines = lines file_contents

  let maybexyss = sequence $ map (fmap fst) $ map (parse parseAnswerValues) file_lines
  case maybexyss of
    Nothing -> print "Nothing"
    Just xyss -> do
      -- Part One
      let operators1 = [(+), (*)]
          vxyss1 = filter (attemptSolve operators1) xyss
       in print $ sum $ map fst vxyss1

      -- Part Two
      let operators2 = [(+), (*), (||)]
          vxyss2 = filter (attemptSolve operators2) xyss
       in print $ sum $ map fst vxyss2

parseAnswerValues :: Parser YXS
parseAnswerValues = do
  result <- numberP
  _ <- characterP ':'
  _ <- whitespaceP
  values <-
    some
      ( do
          value <- numberP
          _ <- whitespaceP
          return value
      )
  return (result, values)

attemptSolve :: [(Int -> Int -> Int)] -> YXS -> Bool
attemptSolve _ (_, []) = undefined
attemptSolve _ (y, (x : [])) = y == x
attemptSolve operators (y, (x1 : x2 : xs)) =
  let x's = map (\op -> x1 `op` x2) operators
   in any (\x' -> attemptSolve operators (y, (x' : xs))) x's