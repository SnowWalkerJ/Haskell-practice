module Parsec where
    import Control.Monad hiding (fail)
    
    data Parser a = Parser (String->(String, Maybe a))
    fail::Parser a
    fail = Parser (\x->(x, Nothing))
    
    instance Functor Parser where
        fmap f parser = do
            a <- parser
            return (f a)

    instance Applicative Parser where
        pure = return
        parser1 <*> parser2 = do
            a <- parser2
            f <- parser1
            return (f a)
    
    instance Monad Parser where
        return a = Parser (\x->(x, Just a))
        parser >>= f = Parser parser' where
            parser' s = let
                (s', a) = runParser parser s
                in case a of
                    Nothing -> (s', Nothing)
                    _ -> let (Just a') = a
                         in runParser (f a') s'

    (<|>)::Parser a->Parser a->Parser a
    parser1 <|> parser2 = Parser parser3 where
        parser3 s = let (s', result) = runParser parser1 s
                    in case result of
                            Nothing -> runParser parser2 s'
                            _ -> (s', result)
        

    runParser::Parser a->String->(String, Maybe a)
    runParser (Parser parser) source = parser source


    parse::Parser a->String->Maybe a
    parse parser source = let (s', result) = runParser parser source
                          in result

    try::Parser a->Parser a
    try parser = Parser parser' where
        parser' s = let (s', result) = runParser parser s
                    in case result of
                        Nothing -> (s, Nothing)
                        _ -> (s', result)

    char::Char->Parser Char
    char ch = Parser f where
        f [] = ([], Nothing)
        f s@(h:xs)| h == ch = (xs, Just h)
                  | otherwise = (xs, Nothing)
                
    string::String->Parser String
    string [] = return ""
    string (h:hs) = do
        x <- char h
        xs <- string hs
        return (x:xs)

    many::Parser a->Parser[a]
    many parser = (do
        result <- try parser
        results <- many parser
        return (result:results)
        ) <|> return []
        
    many1::Parser a->Parser[a]
    many1 parser = do
        result <- try parser
        results <- many parser <|> return []
        return (result:results)
                                        
    oneOf::String->Parser Char
    oneOf lst = Parser parser where
        parser [] = ([], Nothing)
        parser s@(h:xs) | h `elem` lst = (xs, Just h)
                        | otherwise = (s, Nothing)
                      
    noneOf::String->Parser Char
    noneOf lst = Parser parser where
        parser [] = ([], Nothing)
        parser s@(h:xs) | not (h `elem` lst) = (xs, Just h)
                        | otherwise = (s, Nothing)
                      
    sepBy::Parser a->Parser b->Parser [a]
    sepBy parserA parserB = do
        result <- parserA
        results <- (parserB >> sepBy parserA parserB) <|> return []
        return (result:results)
        
    endBy::Parser a->Parser b->Parser [a]
    endBy parserA parserB = do
        result <- parserA
        parserB
        results <- (try $ endBy parserA parserB) <|> return []
        return (result:results)

    sepEndBy::Parser a->Parser b->Parser [a]
    sepEndBy parserA parserB = do
        result <- parserA
        results <- (try parserB>>((try $ sepEndBy parserA parserB)<|> return []))<|>return []
        return (result:results)

    feed::String->Parser a
    feed x = Parser parser where
        parser s = (x++s, Just undefined)



