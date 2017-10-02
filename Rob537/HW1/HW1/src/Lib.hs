module Lib
  (
    trainNetwork
  , classifyTest
  , generateExps
  , runExperiment
  , runExpsIO
  , runExpIO
  ) where

import Numeric.LinearAlgebra

import Net
import CSVMatrixParser

import Data.List
import Text.Printf

type LearningRate = Double
type Epochs = Int
type HiddenUnits = Int
type Sigmoid = Bool

data Experiment = Experiment LearningRate Epochs HiddenUnits Sigmoid deriving (Show, Eq)

latexExp :: Experiment -> String
latexExp (Experiment lr e hu s) = show hu ++ " & " ++ show e ++ " & "
                                  ++ (printf "%.3f" lr :: String) ++ " & " ++ (if s then "Sigmoid" else "Tanh")
                                

latexResults :: [Double] -> String
latexResults results = foldl (\str r -> str ++ " & " ++ (printf "%.2f" (100 * r) :: String) ++ "\\%") "" results
                             
generateExps :: [LearningRate] -> [Epochs] -> [HiddenUnits] -> [Sigmoid] -> [Experiment]
generateExps lrs es hus ss = [Experiment lr e hu s | lr <- lrs, e <- es, hu <- hus, s <- ss]

runExpIO :: String -> [String] -> Experiment -> IO ()
runExpIO trainData testData exp = do
  results1 <- runExperiment trainData testData exp
  results2 <- runExperiment trainData testData exp
  results3 <- runExperiment trainData testData exp
  let avgResults = zipWith3 (\a b c -> (a + b + c) / 3.0) results1 results2 results3
  putStrLn (latexExp exp ++ latexResults avgResults ++ "\\\\")

runExpsIO :: String -> [String] -> [Experiment] -> IO ()
runExpsIO trainData testData = mapM_ (runExpIO trainData testData)
  
runExperiment :: String -> [String] -> Experiment -> IO [Double]
runExperiment trainData testData (Experiment lr e hu s)  = do
  network <- trainNetwork trainData lr e hu s
  sequence $ map (classifyTest network) testData

trainNetwork :: String -> LearningRate -> Epochs -> HiddenUnits -> Sigmoid -> IO NN
trainNetwork trainingData lr epochs nh isSigmoid = do
  let (f, f') = if isSigmoid then (sigmoid, sigmoid') else (tanh, tanh')
  (i,o) <- getData trainingData
  net   <- buildNetwork (f,f') [5,nh,2]
  return $ repTrain epochs lr i o net

classifyTest :: NN -> String -> IO Double
classifyTest net testData = do
  (i,o) <- getData testData
  let output = forward net i
      testRows = toRows o
      expRows = toRows output
      testClass = map classification testRows
      expClass = map classification expRows
      correct = zipWith xnor testClass expClass
      numCorrect = length $ filter id correct
      percentageCorrect = (fromIntegral numCorrect) / (fromIntegral $ length correct)
  return percentageCorrect
  
classification :: Vector R -> Bool
classification vec = vec ! 0 < 0.5 && vec ! 1 > 0.5

xnor :: Bool -> Bool -> Bool
xnor True True = True
xnor False False = True
xnor _ _ = False
