module Lib
    -- ( buildNetwork
    -- , forward
    -- , forwardWithDeriv
    -- , backprop
    -- , backpropLayer
    -- , sigmoid
    -- , sigmoid'
    -- , printNet
    -- , repTrain
    -- , train
    -- )
    where

import Numeric.LinearAlgebra
import Data.Traversable

type ActivationF = Double -> Double
type ActivationD = Double -> Double
type Activation = (ActivationF, ActivationD)
type Bias = Matrix R
type Weights = Matrix R
type Layer = (Weights, Bias, Activation)
type NN = [Layer]

type Input = Vector R
type Output = Vector R
type Deriv = Vector R
type Error = Vector R

repTrain :: Int -> LearningRate -> [(Input, Output)] -> NN -> NN
repTrain 0 _ _ nn = nn
repTrain n lr ios nn = repTrain (n-1) lr ios (train lr ios nn)

train :: LearningRate -> [(Input, Output)] -> NN -> NN
train lr ios net = foldl (\nn (i,o) -> backprop lr i o nn) net ios
    
buildNetwork :: Activation -> [Int] -> IO NN
buildNetwork act (a:b:xs) = do
  weights <- randn b a
  bias    <- randn b 1
  rest    <- buildNetwork act (b:xs)
  return $ (weights, bias, act):rest
buildNetwork _ _ = return []

forward :: NN -> Input -> Output
forward net x = fst $ forwardWithDeriv x net

layerForward :: Input -> Layer -> (Output, (Input, Deriv))
layerForward x (w, b, act) = (output, (x, deriv))
  where
    output = (activate act . (+) b . (<>) w . asColumn) x
    deriv = cmap (snd act) output

forwardWithDeriv :: Input -> NN -> (Output, [(Input, Deriv)])
forwardWithDeriv = mapAccumL layerForward

activate :: Activation -> Matrix R -> Vector R
activate act = flatten . cmap (fst act)

backprop :: LearningRate -> Input -> Output -> NN -> NN
backprop lr input y net = backpropLayers lr error inpDs net
   where
     (output, inpDs) = forwardWithDeriv input net
     error = y - output

backpropLayers :: LearningRate -> Error -> [(Input, Deriv)] -> NN -> NN
backpropLayers lr error inpDs weights = snd $ mapAccumR (backpropLayerTuple lr) error (zip inpDs weights)

backpropLayerTuple :: LearningRate -> Error -> ((Input, Deriv), Layer) -> (Error, Layer)
backpropLayerTuple lr error (inpD, layer_weights) = backpropLayer lr error inpD layer_weights

type LearningRate = Double

backpropLayer :: LearningRate -> Error -> (Input, Deriv) -> Layer -> (Error, Layer)
backpropLayer lr error (layer_inp, layer_deriv) layer = (prev_error, new_layer)
  where
    (layer_weights, layer_bias, act) = layer
    new_layer = (updated_weights, new_bias, act)
    
    d_layer = layer_deriv * error
    updated_weights = (+) layer_weights $ cmap (* lr) ((asColumn d_layer) <> (asRow layer_inp))
    prev_error = flatten $ (<>) (tr layer_weights) (asColumn d_layer)
    new_bias = (+) layer_bias . asColumn . cmap (* lr) $ d_layer
    

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

printNet :: NN -> IO ()
printNet layers = do
  mapM_ (\(w,b,_) -> do
            disp 2 w
            disp 2 b) layers
                 
