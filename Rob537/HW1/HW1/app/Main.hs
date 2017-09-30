module Main where

import Lib
import Numeric.LinearAlgebra
import Data.Traversable

buildTestNetwork :: NN
buildTestNetwork = [hidden, output]
  where
    hidden = (hw, hb, (sigmoid, sigmoid'))
    output = (ow, ob, (sigmoid, sigmoid'))
    hw = (4><3) [0.42,0.88,0.55,
                 0.1,0.73,0.68,
                 0.60,0.18,0.47,
                 0.92,0.11,0.52]
    hb = (1><3) [0.46,0.72,0.08]
    ow = (3><1) [0.3,0.25,0.23]
    ob = (1><1) [0.69]

xor_input = (4><2) [0,0,
                    1,0,
                    0,1,
                    1,1] :: Matrix R
xor_output = (4><1) [0,1,1,0] :: Matrix R

input = (3><4) [1,0,1,0,1,0,1,1,0,1,0,1] :: Matrix R
output = (3><1) [1,1,0] :: Matrix R

oo = (vector [0,0], vector [0])
ox = (vector [0,1], vector [1])
xo = (vector [1,0], vector [1])
xx = (vector [1,1], vector [0])
main :: IO ()
main = do
  -- net <- buildNetwork (relu, relu') [2,2,1]
  -- putStrLn "First XOR Values: "
  -- putStrLn $ show (forward net xor_input)
  -- putStrLn "Initial network: "
  -- printNet net
  -- let trained = repTrain 10000 0.1 xor_input xor_output net
  -- putStrLn "Post Network:"
  -- printNet trained
  -- putStrLn "Final XOR Values:"
  -- putStrLn $ show (forward trained xor_input)

  net' <- buildNetwork (sigmoid, sigmoid') [4,3,1] --buildTestNetwork
  let trained' = repTrain 5000 0.1 input output net'

  putStrLn $ show (forward net' input)
  putStrLn $ show (forward trained' input)
