module Day3 where

import Control.Applicative ((<|>))
import Parser (Parser (..), characterP, keywordP, numberP)

main :: IO ()
main = do
  file_contents <- readFile "data/D3.txt"

  -- Part One
  let mMul = parse (parseInstances mulP) file_contents
   in execIO mMul

  -- Part Two
  let mInst = parse (parseInstances (mulP <|> doP <|> don'tP)) file_contents
   in execIO mInst

data MMInst = Mul Int Int | Do | Don't deriving (Show)

mulP :: Parser MMInst
mulP = do
  _ <- keywordP "mul"
  _ <- characterP '('
  num1 <- numberP
  _ <- characterP ','
  num2 <- numberP
  _ <- characterP ')'
  return $ Mul num1 num2

doP :: Parser MMInst
doP = keywordP "do()" >> return Do

don'tP :: Parser MMInst
don'tP = keywordP "don't()" >> return Don't

parseInstances :: Parser a -> Parser [a]
parseInstances parser = Parser $ \input1 -> do
  case input1 of
    [] -> (Just ([], []))
    _ -> case parse parser input1 of
      Just (result, remainder) -> do
        (more, _) <- parse (parseInstances parser) remainder
        Just (result : more, [])
      Nothing -> do
        let (_ : input2) = input1
        parse (parseInstances parser) input2

-- (runTotal, enable)
type MMEnv = (Int, Bool)

newtype MMState a = MMState {exec :: MMEnv -> Maybe (a, MMEnv)}

instance Functor MMState where
  fmap f (MMState s) = MMState $ \state ->
    case s state of
      Nothing -> Nothing
      Just (a, newState) -> Just (f a, newState)

instance Applicative MMState where
  pure a = MMState $ \state -> Just (a, state)
  (MMState sa) <*> (MMState sb) = MMState $ \state ->
    case sa state of
      Nothing -> Nothing
      Just (a, state') -> case sb state' of
        Nothing -> Nothing
        Just (b, state'') -> Just (a b, state'')

instance Monad MMState where
  return = pure
  (>>=) :: MMState a -> (a -> MMState b) -> MMState b
  (MMState sa) >>= f = MMState $ \state ->
    case sa state of
      Nothing -> Nothing
      Just (a, state') -> exec (f a) state'

get :: MMState MMEnv
get = MMState $ \state -> Just (state, state)

modify :: (MMEnv -> MMEnv) -> MMState ()
modify f = MMState $ \state -> Just ((), f state)

active :: MMState Int
active = MMState $ \(total, enable) -> Just (if enable then 1 else 0, (total, enable))

execInst :: MMInst -> MMState ()
execInst inst = case inst of
  Do -> modify (\(total, _) -> (total, True))
  Don't -> modify (\(total, _) -> (total, False))
  Mul a b -> modify (\(total, enable) -> (total + (a * b * (if enable then 1 else 0)), enable))

execAll :: [MMInst] -> MMState Int
execAll instrs = do
  mapM_ execInst instrs
  (total, _) <- get
  return total

execIO :: Maybe ([MMInst], a) -> IO ()
execIO Nothing = putStrLn "Parse Error"
execIO (Just (inst, _)) = do
  case exec (execAll inst) (0, True) of
    Nothing -> putStrLn "Execution Error"
    Just (total, _) -> print total