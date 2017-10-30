{-# OPTIONS_GHC -XBangPatterns #-}

module Main where

import Lib
import qualified Data.Vector as V
import System.Environment
import Data.List
import Control.Monad

-- cities = V.fromList $ map Location [(1.0,1), (0,0), (1,1), (0,1)]
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Median
median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

-- |Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- |Sample variance
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)

           
expr runs cities sol = do
  finalSol <- runSimAnnealing runs cities sol
  putStrLn $ "Running experiment..."
  putStrLn $ "Final solution: " ++ (show finalSol)
  putStrLn $ "Final score: " ++ (show (eval cities finalSol))
  return (finalSol, eval cities finalSol)
  
main :: IO ()
main = do
  args <- getArgs
  let csv = args !! 0
      runs = read (args !! 1) :: Int
      stat_runs = read (args !! 2) :: Int

  cities <- getCSV csv
  putStrLn $ show cities
  
  --let sol = [0..(length cities - 1)]
  --results <- replicateM stat_runs (expr runs cities sol)

  --let scores = map snd results
  --    m = mean scores
  --    med = median scores
  --    std = stddev scores
  --    conf = 1.96 * std / (sqrt (fromIntegral stat_runs))
  --    lower = m - conf
  --    upper = m + conf
  --putStrLn $ "Finished experiment. Ran " ++ (show stat_runs) ++ " statistical runs."
  --putStrLn $ "Mean: " ++ (show m)
  --putStrLn $ "Median: " ++ (show med)
  --putStrLn $ "Stddev: " ++ (show std)
  --putStrLn $ "Minimum: " ++ (show (minimum scores))
  --putStrLn $ "95 percent Confidence Interval: " ++ (show conf)
  --putStrLn $ "Lower interval: " ++ (show lower)
  --putStrLn $ "Upper interval: " ++ (show upper)
