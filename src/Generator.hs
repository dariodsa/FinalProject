module Generator where


data Dot = Dot Double Double deriving (Show, Eq)

type FileName = String


generateDots :: IO ()
generateDots = do
              numberOfDots <- getInputInfo
              putStrLn $ "I will create " ++ show numberOfDots ++ " dots and save it to dots1.txt"
              saveDots "dots1.txt" $ createDots numberOfDots
              return ()
            
getInputInfo :: IO Int
getInputInfo = do 
            putStrLn "I will generate completly random 2-D dot (in geo coordinate style)."
            putStrLn "Please enter the number of dots." 
            number <- getLine
            putStrLn "Great"
            return (read number :: Int)

createDots :: Int -> [Dot]
createDots _ = []

saveDots :: FileName -> [Dot] -> IO ()
saveDots filePath xs = undefined
