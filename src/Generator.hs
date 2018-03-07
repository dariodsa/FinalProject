module Generator where

import System.Random (randomRIO)

data Dot = Dot {xCor :: Double,
                yCor :: Double}
                deriving (Show, Eq)

type FileName = String


generateDots :: IO ()
generateDots = do
              putStrLn "I will generate completly random 2-D dot (in geo coordinate style)."
              putStrLn "Please enter the number of dots." 
              newLine <- getLine
              numberOfDots <- getLine
              putStrLn $ "I will create " ++ show numberOfDots ++ " dots and save it to dots1.txt"
              xs <- createDots (read numberOfDots :: Int)
              ys <- createDots (read numberOfDots :: Int)
              saveDots "dots1.txt" xs ys
              return ()


createDots :: Int -> IO([Double])
createDots  0  = return []
createDots num = do
                  x <- randomRIO(-90.0 , 90.0)
                  --y <- randomRIO(-180,0, 180.0)
                  xs <- createDots (num-1)
                  --dot <- Dot x y 
                  return (x:xs)
                

saveDots :: FileName -> [Double] -> [Double] -> IO ()
saveDots filePath xs ys = writeFile filePath (dotsToString xs ys)

dotsToString :: [Double] -> [Double] -> String
dotsToString [] [] = ""
dotsToString (x:xs) (y:ys) = show x ++ "," ++ show y ++ "\n" ++ dotsToString xs ys
--dotsToString = foldl  (\acc x -> acc ++ getX x ++ "," ++ getY x ++"\n") ""
--            where getX = show . xCor 
--                  getY = show . yCor