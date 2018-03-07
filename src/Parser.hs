module Parser where

parsingFile :: IO()
parsingFile = do
         putStrLn "File  location, please"
         fileLoc <- getLine
         putStrLn fileLoc