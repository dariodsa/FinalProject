module Generator where

import System.Random (randomRIO)
import Control.Monad (foldM)
import Data.Char (chr)

data Dot = Dot {xCor :: Double,
                yCor :: Double}
                deriving (Show, Eq)

type FileName = String


generateDots :: IO ()
generateDots = do
              putStrLn "I will generate completly random 2-D dot (in geo coordinate style)."
              putStrLn "Please enter the number of dots. * 100000" 
              newLine <- getLine
              numberOfDots <- getLine
              putStrLn $ "I will create " ++ show numberOfDots ++ " dots and save it to dots1.txt"
              let br = chr(read numberOfDots :: Int)
              let xs = ['\0'..br]
              putStrLn $ show $ length xs
              rez <- foldM (\ac x -> addDots 1000) () xs
              return rez
--              return ()

addDots :: Int -> IO ()
addDots numberOfDots = do 
              xs <- createDots numberOfDots  (-90.0)   90.0
              ys <- createDots numberOfDots  (-180.0)  180.0
              putStrLn $ show numberOfDots
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
                      
                      dotsToString xs ys filePath
                      return ()

dotsToString :: [Double] -> [Double] -> FileName -> IO ()
dotsToString [] []  _ = return ()
dotsToString (x:xs) (y:ys) filePath = do 
                          appendFile filePath (show x ++ "," ++ show y ++ "\n")
                          dotsToString xs ys filePath
                          return ()
