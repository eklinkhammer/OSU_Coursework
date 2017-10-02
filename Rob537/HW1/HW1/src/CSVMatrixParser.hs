{-# LANGUAGE ScopedTypeVariables #-}

module CSVMatrixParser
  (
    getData
  ) where

import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

getCSV :: String -> IO (V.Vector [Double])
getCSV path = do
  csvData <- BL.readFile path
  case decode NoHeader csvData of
    Left err -> return V.empty
    Right v -> return v

splitIO :: [Double] -> ([Double], [Double])
splitIO list = splitAt 5 list

toVectorRow :: ([Double], [Double]) -> (Vector R, Vector R)
toVectorRow (xs,ys) = (vector xs, vector ys)

concatMats :: V.Vector (Vector R, Vector R) -> (Matrix R, Matrix R)
concatMats vec = let list = V.toList vec
                 in (fromRows (map fst list), fromRows (map snd list))

getData :: String -> IO (Matrix R, Matrix R)
getData path = getCSV path >>= return . concatMats . fmap (toVectorRow . splitIO)

concatTupleMats :: (Matrix R, Matrix R) -> (Matrix R, Matrix R) -> (Matrix R, Matrix R)
concatTupleMats (a,x) (b,y) = (a === b, x === y)

