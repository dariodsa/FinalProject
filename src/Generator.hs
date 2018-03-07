module Generator where

import System.Random (randomR, getStdRandom)
import System.IO.Unsafe

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
              saveDots "dots1.txt" ( createDots ( read numberOfDots :: Int)) 
              return ()


createDots :: Int -> [Dot]
createDots  0  = []
createDots num =  [ 
                  Dot ( unsafePerformIO (getStdRandom (randomR (-90.0 , 90.0))))
                   (unsafePerformIO (getStdRandom (randomR (-180.0, 180.0))))
                  ] ++ createDots (num-1)
                

saveDots :: FileName -> [Dot] -> IO ()
saveDots filePath xs = writeFile filePath (dotsToString xs)

dotsToString :: [Dot] -> String
dotsToString = foldl  (\acc x -> acc ++ getX x ++ "," ++ getY x ++"\n") ""
            where getX = show . xCor 
                  getY = show . yCor