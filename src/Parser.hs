{-# LANGUAGE LambdaCase #-}

module Parser (Parser (..), commaSeparated, semicolonSeparated, keywordP, characterP, whitespaceP, whitespaceRP, numberP, boolP, identifierP, characterSeparated, newline, parserSeparated, alphaNumP) where

import Control.Applicative (Alternative (empty, (<|>)), many, some)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> case p input of
    Just (x, rest) -> Just (f x, rest)
    Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (x, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser f) <*> (Parser p) = Parser $ \input -> case f input of
    Nothing -> Nothing
    Just (g, rest) -> case p rest of
      Nothing -> Nothing
      Just (x, r) -> Just (g x, r)

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input -> case p input of
    Nothing -> Nothing
    Just (x, rest) -> parse (f x) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Nothing -> p2 input
    result -> result

-- Generalised Parsers
characterSeparated :: Char -> Parser a -> Parser [a]
characterSeparated c parser =
  ( do
      first <- parser
      rest <-
        many
          ( do
              _ <- whitespaceP
              _ <- characterP c
              _ <- whitespaceP
              parser
          )
      return (first : rest)
  )
    <|> return []

parserSeparated :: Parser b -> Parser a -> Parser [a]
parserSeparated separatorParser parser =
  ( do
      first <- parser
      rest <-
        many
          ( do
              _ <- whitespaceP
              _ <- separatorParser
              _ <- whitespaceP
              parser
          )
      return (first : rest)
  )
    <|> return []

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = characterSeparated ',' parser

semicolonSeparated :: Parser a -> Parser [a]
semicolonSeparated parser = characterSeparated ';' parser

keywordP :: String -> Parser String
keywordP s = do
  Parser $ \input ->
    if s == take (length s) input
      then Just (s, drop (length s) input)
      else Nothing

characterP :: Char -> Parser Char
characterP c = Parser $ \ns -> case ns of
  (x : xs) | x == c -> Just (x, xs)
  _ -> Nothing

spaceP :: Parser Char
spaceP = Parser $ \ns -> case ns of
  (x : xs) | isSpace x -> Just (x, xs)
  _ -> Nothing

whitespaceP :: Parser ()
whitespaceP = void $ many spaceP

whitespaceRP :: Parser ()
whitespaceRP = void $ some spaceP

numberP :: Parser Int
numberP =
  fmap
    read
    ( some $ Parser $ \ns -> case ns of
        (x : xs) | isDigit x -> Just (x, xs)
        _ -> Nothing
    )

boolP :: Parser Bool
boolP = (keywordP "true" >> return True) <|> (keywordP "false" >> return False)

alphaP :: Parser Char
alphaP = Parser $ \ns -> case ns of
  (x : xs) | isAlpha x -> Just (x, xs)
  _ -> Nothing

alphaNumP :: Parser Char
alphaNumP = Parser $ \ns -> case ns of
  (x : xs) | isAlphaNum x -> Just (x, xs)
  _ -> Nothing

identifierP :: Parser String
identifierP = do
  firstChar <- alphaP
  remChars <- many alphaNumP
  return (firstChar : remChars)

newline :: Parser Char
newline = characterP '\n'