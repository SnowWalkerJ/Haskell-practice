module Markdown where
    import Prelude hiding (fail)
    import Parsec
    import Control.Monad hiding (fail)

    anything::Parser String
    anything = many (noneOf "\n")

    -- <b>
    boldParser::Parser String
    boldParser = do
        string "**"
        content <- many (noneOf "*\n")
        string "**"
        return $ "<b>" ++ content ++ "</b>"

    -- <i>
    italicParser::Parser String
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
    headParser::Parser String
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
        else fail

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
    ulParser::Int->Parser String
    ulParser upLevel = do
        n <- countParser (char '\t')
        if n < upLevel then fail else char '-'
        many $ oneOf " \t"
        thisItem <- contentParser
        
        items <- try (char '\n' >>endBy (try (liParser n) <|> try(ulParser n)) (char '\n')) <|> return [""]
        let indent = replicate n '\t'
            itemHTML = indent ++ "\t<li>" ++ thisItem ++ "</li>\n"
        feed "\n"
        return $ indent ++ "<ul>\n" ++ itemHTML ++ concat items ++ indent ++ "</ul>\n"

    -- <li>
    liParser n = do
        string $ replicate n '\t'
        char '-'
        many (oneOf " \t")
        item <- contentParser
        let indent = replicate (n+1) '\t'
        return $ indent ++ "<li>" ++ item ++ "</li>\n"
------------- List  ---------------
------------- Table ---------------
    data Align = AlignLeft | AlignCenter | AlignRight
    instance Show Align where
        show AlignLeft = "left"
        show AlignCenter = "center"
        show AlignRight = "right"
    tableParser::Parser String
    tableParser = do
        titles  <- titleParser
        char '\n'
        aligns  <- alignParser
        char '\n'
        rows    <- sepEndBy rowParser (char '\n')
        let tr row = "<tr>\n" ++ concatMap td alignedRow ++ "</tr>\n"
                where alignedRow = zip aligns row
            td (align, col) = "\t<td class=\"" ++ show align ++ "\">" ++ col ++ "</td>\n"
            th (align, col) = "\t<th class=\"" ++ show align ++ "\">" ++ col ++ "</th>\n"
        return $ "<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">\n<tr>\n" ++ concatMap th (zip aligns titles) ++ "</tr>\n" ++ concatMap tr rows ++ "</table>"

    titleParser::Parser [String]
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

------------- Header --------------
    html content = "<!DOCTYPE html>\n<html>\n" ++ header ++ body content ++ "</html>"
    header = "<head>\n" ++ 
        "<style type=\"text/css\">\n" ++
        ".center {text-align: center;}\n" ++
        ".left {text-align: left;}\n" ++
        ".right {text-align: right;}\n" ++
        "table {border-collapse:collapse;}\n</style>\n</head>\n"
    body content = "<body>" ++ content ++ "</body>"

------------- Header --------------

    parser = do 
        parsed <- flip sepEndBy (char '\n') $ 
            try hrParser
            <|> try tableParser
            <|> try (ulParser 0)
            <|> try headParser 
            <|> try paragraphParser
            <|> return ""
        return $ concat parsed

    parseMarkdown::String->String
    parseMarkdown markdown = 
        let parsed = parse parser markdown
        in case parsed of
            Nothing -> "ParseError"
            Just result -> html result

