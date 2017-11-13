module Lib
    ( successor
    , eval
    , simulatedAnnealing
    , nextT
    , nextS
    , Location (..)
    , prob
    , swapSuccessor
    , runSimAnnealing
    , getCSV
    , runEA
    , epsilonGreedy
    ) where

import System.Random
import System.Random.Shuffle
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Control.Monad
import Data.List

import CCEA

type Solution = [Int]

type Cities = V.Vector Location
type Temperature = Double
newtype Location = Location (Double, Double)
  deriving (Eq)

instance Show Location where
  show (Location (x,y)) = show (x,y)

instance Num Location where
  (+) (Location (x,y)) (Location (a,b)) = Location (x+a, y+b)
  (*) (Location (x,y)) (Location (a,b)) = Location (x*a, y*b)
  abs (Location (x,y)) = Location (abs x, abs y)
  signum (Location (x,y)) = Location (signum x, signum y)
  fromInteger int = Location (fromInteger int, fromInteger int)
  negate (Location (x,y)) = Location (negate x, negate y)

runSimAnnealing :: Int -> Cities -> Solution -> IO Solution
runSimAnnealing n cities sol = simulatedAnnealing n cities n 100 sol

runEA :: Int -> Cities -> IO Solution
runEA n cities = do
  ea <- getEA cities
  gen <- getStdGen
  let (EA pop _ _ _) = snd $ evolveN n gen ea
      result = sortOn (eval cities) pop
  return $ head result

epsilonGreedy :: Int -> Double -> Cities -> Solution -> IO Solution
epsilonGreedy 0 _ _ sol = return sol
epsilonGreedy n eps cities sol = do
  potentialCities <- replicateM 50 (swapSuccessorIO sol)
  let bestRoute = head $ sortOn (eval cities) potentialCities

  p <- randomIO :: IO Double
  let nextSol = if eval cities bestRoute < eval cities sol
                then bestRoute
                else if p < eps then bestRoute else sol
  epsilonGreedy (n-1) eps cities nextSol

simulatedAnnealing :: Int -> Cities -> Int -> Temperature -> Solution -> IO Solution
simulatedAnnealing _ _ 0 _ sol = return sol
simulatedAnnealing k cities n t sol = do
  nextSol <- nextS cities (nextT k t) sol
  simulatedAnnealing k cities (n-1) t nextSol

nextT :: Int -> Temperature -> Temperature
nextT k t = t * (0.95 ** (fromIntegral k))
  
nextS :: Cities -> Temperature -> Solution -> IO Solution
nextS cities t sol = do
  potential <- swapSuccessorIO sol
  
  let p = prob t score pot_score
      pot_score = eval cities potential
      score = eval cities sol
  r <- randomIO :: IO Double
  return $ if pot_score < score
           then potential
           else if r < p then potential else sol
  
prob :: Temperature -> Double -> Double -> Double
prob t s1_score s2_score = exp (negate dE / t)
  where
    dE = s2_score - s1_score
    
-- Simulated annealing
successor :: Solution -> IO Solution
successor solution = do
  gen <- getStdGen
  return $ shuffle' solution (length solution) gen

swapSuccessorIO :: Solution -> IO Solution
swapSuccessorIO solution = do
  pos  <- randomRIO (0, max 0 $ length solution - 1)
  next <- randomRIO (0, max 0 $ length solution - 1)
  return $ if pos < next
           then swapElementsAt pos next solution
           else if pos == next then solution
                else swapElementsAt next pos solution

swapSuccessor :: RandomGen g => g -> Solution -> (g, Solution)
swapSuccessor g solution = (gen', nextSol)
  where
    (pos, g')    = randomR (0, max 0 $ length solution - 1) g
    (next, gen') = randomR (0, max 0 $ length solution - 1) g'
    nextSol = if pos < next
              then swapElementsAt pos next solution
              else if pos == next then solution
                   else swapElementsAt next pos solution

tspFitness :: Cities -> FitnessFunction Solution
tspFitness cities = eval cities

tspBreeding :: (RandomGen g) => BreedingStrategy Solution g 
tspBreeding g = mapAccumL swapSuccessor g


getEA :: Cities -> IO (EA Solution StdGen)
getEA cities = do
  initSols <- replicateM 50 (successor ([0..(length cities - 1)]))
  return $ EA initSols fitnessProp (tspFitness cities) (elitist tspBreeding)
  
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = let elemI = xs !! i
                            elemJ = xs !! j
                            left = take i xs
                            middle = take (j - i - 1) (drop (i + 1) xs)
                            right = drop (j + 1) xs
                        in  left ++ [elemJ] ++ middle ++ [elemI] ++ right
eval :: Cities -> Solution -> Double
eval cities = eval' cities . circle

eval' :: Cities -> Solution -> Double
eval' cities (a:b:xs) = distance (cities V.! a) (cities V.! b) + eval' cities (b:xs)
eval' cities _ = 0

distance :: Location -> Location -> Double
distance loc1 loc2 = norm (loc2 - loc1)

norm :: Location -> Double
norm loc = let (Location (x2,y2)) = loc * loc in sqrt (x2+y2)

circle :: Solution -> Solution
circle solution = ((last solution):solution)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getCSV :: String -> IO (V.Vector Location)
getCSV path = do
  csvData <- BL.readFile path
  case decode HasHeader csvData of
    Left err -> return V.empty
    Right v -> return $ V.map toLocation v

toLocation :: [Double] -> Location
toLocation (x:y:_) = Location (x,y)
toLocation _ = undefined

