module Information 
            (
              MoveType,
              StructureType,
              Result
            ) where

data StructureType = Bucket | BBT deriving (Read)
                   
data MoveType = BigMove 
              | MediumMove
              | SmallMove deriving (Read)

data Result = Result {
                     structureType  :: StructureType,
                     queryTime      :: Double, 
                     moveTime       :: Double,
                     relocationTime :: Double,
                     numOfWorkers   :: Int,
                     moveType       :: MoveType,
                     bucketSize     :: Int
                   }  
                    