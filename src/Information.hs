module Information 
            (
              MoveType,
              StructureType,
              Result
            ) where

data StructureType = Bucket | BBT deriving (Read, Show, Ord, Eq)
data MoveType = SmallMove 
              | MediumMove
              | BigMove deriving (Read, Show, Ord, Eq)

data Result = Result {
                     structureType  :: StructureType,
                     queryTime      :: Double, 
                     moveTime       :: Double,
                     relocationTime :: Double,
                     numOfWorkers   :: Int,
                     moveType       :: MoveType,
                     bucketSize     :: Int
                   }  
                    