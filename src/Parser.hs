module Parser(Parser(..), errMsg, commaSeparated, semicolonSeparated, keywordP, characterP, whitespaceP, whitespaceRP, numberP, boolP, identifierP, characterSeparated, newline, parserSeparated) where
    import Control.Applicative (Alternative(empty, (<|>)), many, some)
    import Data.Char (isAlphaNum, isAlpha, isSpace, isDigit)
    import Control.Monad (void)
    
    newtype Parser a = Parser {parse :: String -> Either String (a, String)}

    errMsg :: String -> String -> Either String a
    errMsg err ns = Left (err ++ "'" ++ (take 20 $ takeWhile (\c -> c /= '\n') ns) ++ "...'")

    instance Functor Parser where
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap f (Parser p) = Parser $ \input -> case p input of
            Left err -> Left err
            Right (x, rest) -> Right (f x, rest)

    instance Applicative Parser where
        pure :: a -> Parser a
        pure x = Parser $ \input -> Right (x, input)
        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        (Parser f) <*> (Parser p) = Parser $ \input -> case f input of
            Left err -> Left err
            Right (g, rest) -> case p rest of
                Left err -> Left err
                Right (x, r) -> Right (g x, r)

    instance Monad Parser where
        return :: a -> Parser a
        return = pure
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        (Parser p) >>= f = Parser $ \input -> case p input of
            Left err -> Left err
            Right (x, rest) -> parse (f x) rest

    instance Alternative Parser where
        empty :: Parser a
        empty = Parser $ const Left "Parse Failed"
        (<|>) :: Parser a -> Parser a -> Parser a
        (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
            Left _ -> p2 input
            result -> result

    -- Generalised Parsers
    characterSeparated :: Char -> Parser a -> Parser [a]
    characterSeparated c parser = 
        (do
            first <- parser
            rest <- many (do
                _ <- whitespaceP
                _ <- characterP c
                _ <- whitespaceP
                parser)
            return (first : rest))
        <|> return []

    parserSeparated :: Parser b -> Parser a -> Parser [a]
    parserSeparated separatorParser parser = 
        (do
            first <- parser
            rest <- many (do
                _ <- whitespaceP
                _ <- separatorParser
                _ <- whitespaceP
                parser)
            return (first : rest))
        <|> return []

    commaSeparated :: Parser a -> Parser [a]
    commaSeparated parser = characterSeparated ',' parser

    semicolonSeparated :: Parser a -> Parser [a]
    semicolonSeparated parser = characterSeparated ';' parser

    keywordP :: String -> Parser String
    keywordP s = do
        Parser $ \input ->
            if s == take (length s) input
            then Right (s, drop (length s) input)
            else errMsg ("expected keyword '" ++ s ++ "', ") input

    characterP :: Char -> Parser Char
    characterP c = Parser $ \ns -> case ns of
        (x:xs) | x == c -> Right (x, xs)
        _ -> errMsg ("expected character '" ++ [c] ++ "', ") ns

    spaceP :: Parser Char
    spaceP = Parser $ \ns -> case ns of
        (x : xs) | isSpace x -> Right (x, xs)
        _ -> errMsg "missing: expected ' ' (space), " ns

    whitespaceP :: Parser ()
    whitespaceP = void $ many spaceP

    whitespaceRP :: Parser ()
    whitespaceRP = void $ some spaceP

    numberP :: Parser Int
    numberP = fmap read (some $ Parser $ \ns -> case ns of
        (x : xs) | isDigit x -> Right (x, xs)
        _ -> errMsg "missing: expected digit, " ns)

    boolP :: Parser Bool
    boolP = (keywordP "true" >> return True) <|> (keywordP "false" >> return False)

    alphaP :: Parser Char
    alphaP = Parser $ \ns -> case ns of
        (x:xs) | isAlpha x -> Right (x, xs)
        _ -> errMsg "missing: expected alphanumeric, " ns

    alphaNumP :: Parser Char
    alphaNumP = Parser $ \ns -> case ns of
        (x:xs) | isAlphaNum x -> Right (x, xs)
        _ -> errMsg "" ns

    identifierP :: Parser String
    identifierP = do
        firstChar <- alphaP
        remChars <- many alphaNumP
        return (firstChar:remChars)

    newline :: Parser Char
    newline = characterP '\n'