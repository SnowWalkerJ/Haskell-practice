module Markdown where
    import Text.ParserCombinators.Parsec
    import Control.Monad

    anything::CharParser () String
    anything = many (noneOf "\n")

    boldParser::CharParser () String
    boldParser = do
        string "**"
        content <- many (noneOf "*\n")
        string "**"
        return $ "<b>" ++ content ++ "</b>"

    italicParser::CharParser () String
    italicParser = do
        char '*'
        content <- many (noneOf "*\n")
        char '*'
        return $ "<i>" ++ content ++ "</i>" 

    contentParser = do
        thisParsed <- try boldParser <|> try italicParser <|> do {c<-noneOf "\n"; return [c]}
        nextParsed <- contentParser <|> return ""
        return $ thisParsed ++ nextParsed

    headParser::CharParser () String
    headParser = do
        --try spaces
        sharps <- many1 (char '#')
        --try space
        let n = length sharps
        content <- contentParser
        return $ "<h" ++ show n ++"><b>" ++ content ++ "</b></h" ++ show n ++ ">\n"

    paragraphParser = do
        content <- contentParser
        return $ "<p>" ++ content ++ "</p>\n"

    parser = do 
        parsed <- flip sepEndBy (char '\n') $ 
            try hrParser 
            <|> try headParser 
            <|> try paragraphParser
            <|> return ""
        return $ concat parsed

    hrParser = do
        s <- many (char '-')
        if length s > 5 then return "<hr />\n"
        else fail ""
    
