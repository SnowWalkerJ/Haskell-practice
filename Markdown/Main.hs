module Main where
    import Markdown
    import Text.ParserCombinators.Parsec    
    main = do 
        content <- getContents
        let parsed = parse parser "Error" content
            prefix = "<!DOCTYPE html>\n<html>\n<body>\n"
            postfix = "</body>\n</html>\n"
        case parsed of
            Left _ -> print "Error"
            Right parsedContent -> putStrLn $ prefix ++ parsedContent ++ postfix

