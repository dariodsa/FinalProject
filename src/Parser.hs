module Parser where

import Information

import Data.List.Split
import qualified Data.Map as Map

data Key = Key StructureType Int Int MoveType
data Value = Value Double Double Double

parsingFile :: IO()
parsingFile = do
         putStrLn "File  location, please"
         newLine <- getLine
         fileLoc <- getLine
         putStrLn "fileLoc will be parsed."
         fileContent <- readFile fileLoc
         let xs = lines fileContent
         return ()

parseMe :: [String] -> [(Key, Value)]
parseMe xs = (\acc x -> [getPair (splitOn "," x )] ++ acc) [] xs

getPair :: [String] -> (Key, Value)
getPair xs = ( key, value) 
        where key   = Key 
                       (read (xs !! 0) :: StructureType)
                       (read (xs !! 1) :: Int)
                       (read (xs !! 2) :: Int)
                       (read (xs !! 3) :: MoveType)
              value = Value 
                       (read (xs !! 0) :: Double)
                       (read (xs !! 1) :: Double)
                       (read (xs !! 2) :: Double)


                     