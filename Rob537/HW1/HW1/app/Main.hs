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

i1 = vector [1,0,1,0]
i2 = vector [1,0,1,1]
i3 = vector [0,1,0,1]
o1 = vector [1]
o2 = vector [1]
o3 = vector [0]


oo = (vector [0,0], vector [0])
ox = (vector [0,1], vector [1])
xo = (vector [1,0], vector [1])
xx = (vector [1,1], vector [0])
main :: IO ()
main = do
  net <- buildNetwork (sigmoid, sigmoid') [2,2,1]
  let x = buildTestNetwork
  printNet x
  putStrLn $ show (forward x i1)
  let i = vector [1,1,1,1]
      (o, info) = forwardWithDeriv i x
      layer = (head (tail x))
      outInfo = (head (tail info))
      (e, l) = backpropLayer 1 (vector [1, 0]) outInfo layer
      nn = backprop 1 i (vector [1,0]) x
  -- putStrLn $ show o
  -- putStrLn $ show info
  -- putStrLn $ show e
  -- printNet [l]
  -- putStrLn $ "Error: " ++ show e
  -- printNet nn
  let input = [oo,ox,xo,xx]
      trained = repTrain 10000 0.01 input net
  putStrLn $ show $ map (forward net . fst) input
  printNet trained
  putStrLn $ show $  map (forward trained . fst) input
