module Parser where

import Information

import qualified Data.List as L
import Data.List.Split
import Data.Map

type QueryTime = Double
type MoveTime = Double
type RelocationTime = Double

type NumOfWorker = Int
type BucketSize = Int

data Key = Key StructureType NumOfWorker BucketSize MoveType deriving (Ord, Eq, Read)
data Value = Value { query :: QueryTime
                   , move  :: MoveTime
                   , reloc :: RelocationTime
                   } deriving (Read, Eq)
data Results = Results { querys :: [QueryTime]
                       , moves  :: [MoveTime]
                       , relocations :: [RelocationTime]
                       } deriving (Read, Ord, Eq)

instance Show Key where 
  show (Key structureType bucketSize numOfWorkers moveType) = 
             show structureType ++ ", " ++ show bucketSize ++ ", " ++ 
             show numOfWorkers ++ ", " ++ show moveType

instance Show Value where
  show (Value x y z) = show x ++ " " ++ show y ++ " " ++ show z 

parsingFile :: IO()
parsingFile = do
         putStrLn "File  location, please"
         newLine <- getLine
         fileLoc <- getLine
         putStrLn "fileLoc will be parsed."
         fileContent <- readFile fileLoc
         let xs = lines fileContent
         putStrLn $ printResults ( parseMe empty xs)
            
printResults :: Map Key Results -> String
printResults m = Prelude.foldl f1 "" (toList m)
             where f1 acc (key,value) =
                            acc ++ "\n" ++ show key ++ " " ++ result
                             where result = show queryRes ++ " " ++ show moveRes ++ " " ++ show relRes
                                   queryRes = stat (querys value)
                                   moveRes  = stat (moves value)
                                   relRes   = stat (relocations value)
                            
                                                 

parseMe :: Map Key Results -> [String] -> Map Key Results
parseMe m = Prelude.foldl acc empty 
         where acc :: Map Key Results -> String -> Map Key Results
               acc xs x = case member key xs of 
                            True  -> update (\a -> Just (Results 
                                                                 (querys a ++ [que])
                                                                 (moves a  ++ [mov])
                                                                 (relocations a ++ [rel])
                                                                 )) key xs
                            False -> insert key (Results [que] [mov] [rel]) xs
                          where pair  = getPair $ splitOn " " x
                                key   = fst pair
                                value = snd pair
                                que = query value
                                mov  = move value
                                rel = reloc value


getPair :: [String] -> (Key, Value)
getPair xs = ( key, value)        
        where key   = Key 
                       (read (xs !! 0) :: StructureType)
                       (read (xs !! 1) :: NumOfWorker)
                       (read (xs !! 2) :: BucketSize)
                       (read (xs !! 3) :: MoveType)
              value = Value 
                       (read (xs !! 4) :: QueryTime)
                       (read (xs !! 5) :: MoveTime)
                       (read (xs !! 6) :: RelocationTime)

getJustValue :: Maybe a -> a
getJustValue (Just x) = x                     

stat :: (Num a, Ord a, Fractional a) => [a] -> a 
stat a = (sum arr) / (fromIntegral( length arr))
       where arr = rmArr a

rmArr :: (Ord a, Eq a) => [a] -> [a]
rmArr arr = take (len2) (L.reverse  (L.sort arrWB))
        where len1 = (length arr) - 1
              len2 = len1 - 1
              arrWB = take len1 (L.sort arr)
