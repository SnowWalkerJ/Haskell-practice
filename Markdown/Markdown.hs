module Markdown where
    import Text.ParserCombinators.Parsec
    import Control.Monad

    anything::CharParser () String
    anything = many (noneOf "\n")

    -- <b>
    boldParser::CharParser () String
    boldParser = do
        string "**"
        content <- many (noneOf "*\n")
        string "**"
        return $ "<b>" ++ content ++ "</b>"

    -- <i>
    italicParser::CharParser () String
    italicParser = do
        char '*'
        content <- many (noneOf "*\n")
        char '*'
        return $ "<i>" ++ content ++ "</i>" 

    -- content
    contentParser = do
        thisParsed <- try boldParser <|> try italicParser <|> do {c<-noneOf "\n"; return [c]}
        nextParsed <- contentParser <|> return ""
        return $ thisParsed ++ nextParsed

    -- <h1> <h2>...
    headParser::CharParser () String
    headParser = do
        many (char ' ')
        sharps <- many1 (char '#')
        try (char ' ') <|> return ' '
        let n = length sharps
        content <- contentParser
        return $ "<h" ++ show n ++"><b>" ++ content ++ "</b></h" ++ show n ++ ">\n"

    -- <p>
    paragraphParser = do
        content <- contentParser
        return $ "<p>" ++ content ++ "</p>\n"

    hrParser = atLeast 5 (char '-') >> return "<hr />\n"

    -- at least n matches of the parser
    atLeast n ps = do
        s <- many ps
        if length s >= n then return s
        else fail ""

    -- returns the number of a particular parser
    countParser parser = do
        s <- many parser
        return $ length s

    -- similar to countParser, except that it fails when zero match
    countParser1 parser = do
        s <- many1 parser
        return $ length s

-------------- List ---------------
    -- <ul>
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

    -- <li>
    liParser n = do
        string $ replicate n '\t'
        char '-'
        many (oneOf " \t")
        item <- contentParser
        char '\n'
        let indent = replicate (n+1) '\t'
        return $ indent ++ "<li>" ++ item ++ "</li>\n"
------------- List  ---------------
------------- Table ---------------
    data Align = AlignLeft | AlignCenter | AlignRight
    tableParser::CharParser () String
    tableParser = do
        titles  <- titleParser
        char '\n'
        aligns  <- alignParser
        char '\n'
        rows    <- sepEndBy rowParser (char '\n')
        let tr row = "<tr>\n" ++ concatMap td row ++ "</tr>\n"
            td col = "\t<td>" ++ col ++ "</td>\n"
            th col = "\t<th>" ++ col ++ "</th>\n"
        return $ "<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">\n<tr>\n" ++ concatMap th titles ++ "</tr>\n" ++ concatMap tr rows ++ "</table>"

    titleParser::CharParser () [String]
    titleParser = char '|' >> endBy (many (noneOf "|\n")) (char '|')
    
    alignParser = char '|' >> endBy (do
        left <- countParser (char ':')
        many1 (char '-')
        right <- countParser (char ':')
        return $ if left > 0 
            then if right > 0 
                then AlignCenter
                else AlignLeft
            else AlignRight
        ) (char '|')

    rowParser = char '|' >> endBy (many (noneOf "|\n")) (char '|')

------------- Table ---------------

    parser = do 
        parsed <- flip sepEndBy (char '\n') $ 
            try hrParser
            <|> try tableParser
            <|> try (ulParser 0)
            <|> try headParser 
            <|> try paragraphParser
            <|> return ""
        return $ concat parsed

