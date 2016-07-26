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
        many (char ' ')
        sharps <- many1 (char '#')
        try (char ' ') <|> return ' '
        let n = length sharps
        content <- contentParser
        return $ "<h" ++ show n ++"><b>" ++ content ++ "</b></h" ++ show n ++ ">\n"

    paragraphParser = do
        content <- contentParser
        return $ "<p>" ++ content ++ "</p>\n"

    parser = do 
        parsed <- flip sepEndBy (char '\n') $ 
            try hrParser 
            <|> try (ulParser 0)
            <|> try headParser 
            <|> try paragraphParser
            <|> return ""
        return $ concat parsed

    hrParser = atLeast 5 (char '-') >> return "<hr />\n"

    atLeast n ps = do
        s <- many ps
        if length s >= n then return s
        else fail ""

    countParser parser = do
        s <- many parser
        return $ length s

    countParser1 parser = do
        s <- many1 parser
        return $ length s

    ulParser::Int->CharParser () String
    ulParser upLevel = do
        n <- countParser (char '\t')
        if n < upLevel then fail "" else char '-'
        many $ oneOf " \t"
        thisItem <- contentParser
        char '\n'
        items <- many (try (liParser n) <|> try(ulParser n)) <|> return [""]
        let indent = replicate n '\t'
            itemHTML = indent ++ "\t<li>" ++ thisItem ++ "</li>\n"
        return $ indent ++ "<ul>\n" ++ itemHTML ++ concat items ++ indent ++ "</ul>\n"

    liParser n = do
        string $ replicate n '\t'
        char '-'
        many (oneOf " \t")
        item <- contentParser
        char '\n'
        let indent = replicate (n+1) '\t'
        return $ indent ++ "<li>" ++ item ++ "</li>\n"

