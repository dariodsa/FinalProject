module Parser where

import Information

import Data.List.Split
import Data.Map

data Key = Key StructureType Int Int MoveType deriving (Ord, Eq)
data Value = Value Double Double Double

instance Show Key where 
  show (Key structureType bucketSize numOfWorkers moveType) = 
            "(" ++ show structureType ++ ", " ++ show bucketSize ++ ", " ++ 
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
         putStrLn $ printResults ( parseMe xs empty)
            
printResults :: Map Key [Value] -> String
printResults m = 
                 show ( Prelude.foldl (\acc x ->
                            show(show x ++ " => " 
                            ++ show (Prelude.foldl (
                                   \(Value acc1 acc2 acc3) (Value x1 x2 x3) -> 
                                    (
                                      let len = fromIntegral(length (getJustValue (Data.Map.lookup x m)))
                                      in Value 
                                      (acc1 + x1 / len)
                                      (acc2 + x2 / len)
                                      (acc3 + x3 / len)
                                  )) (Value 0.0 0.0 0.0)  (getJustValue(Data.Map.lookup x m)))
                            ++ "\n")
                         ) "" (Data.Map.keys m))
                                                 

parseMe :: [String] -> Map Key [Value] -> Map Key [Value]
parseMe arr m = Prelude.foldl acc empty arr
         where acc :: Map Key [Value] -> String -> Map Key [Value] 
               acc xs x = case member key xs of 
                            True  -> update (\a -> Just (a ++ [value])) key xs
                            False -> insert key [value] xs
                          where pair  = getPair $ splitOn "," x
                                key   = fst pair
                                value = snd pair


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

getJustValue :: Maybe a -> a
getJustValue (Just x) = x
                     