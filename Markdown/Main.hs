module Main where
    import Markdown
    import Text.ParserCombinators.Parsec    
    main = do 
        content <- getContents
        putStrLn $ parseMarkdown content

