module Main where
     import Markdown
     main = do
         content <- getContents
         putStrLn $ parseMarkdown content