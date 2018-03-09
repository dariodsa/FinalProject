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
              xs <- createDots (read numberOfDots :: Int)  (-90.0)   90.0
              ys <- createDots (read numberOfDots :: Int) (-180.0)  180.0
              saveDots "dots1.txt" xs ys
              return ()


createDots :: Int -> Double -> Double -> IO([Double])
createDots  0    _   _ = return []
createDots num min max = do
                  x <- randomRIO(min , max)
                  --y <- randomRIO(-180,0, 180.0)
                  xs <- createDots (num-1) min max
                  --dot <- Dot x y 
                  return (x:xs)
                

saveDots :: FileName -> [Double] -> [Double] -> IO ()
saveDots filePath xs ys = do 
                      writeFile filePath ""
                      dotsToString xs ys filePath
                      return ()


dotsToString :: [Double] -> [Double] -> FileName -> IO ()
dotsToString [] []  _ = return ()
dotsToString (x:xs) (y:ys) filePath = do 
                          appendFile filePath (show x ++ "," ++ show y ++ "\n")
                          dotsToString xs ys filePath
                          return ()
