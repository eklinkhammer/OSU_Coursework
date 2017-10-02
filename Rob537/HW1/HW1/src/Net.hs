module Net
  (
    repTrain
  , buildNetwork
  , forward
  , mse
  , sigmoid
  , sigmoid'
  , tanh'
  , printNet
  , NN
  ) where

import Numeric.LinearAlgebra
import Data.Traversable

type ActivationF = Double -> Double
type ActivationD = Double -> Double
type Activation = (ActivationF, ActivationD)
type Bias = Matrix R
type Weights = Matrix R
type Layer = (Weights, Bias, Activation)
type NN = [Layer]

type Input = Matrix R
type Output = Matrix R
type Deriv = Matrix R
type Error = Matrix R
type LearningRate = Double

repTrain :: Int -> LearningRate -> Input -> Output -> NN -> NN
repTrain 0 _ _ _ nn = nn
repTrain n lr x y nn = repTrain (n-1) lr x y (backprop lr x y nn)
    
buildNetwork :: Activation -> [Int] -> IO NN
buildNetwork act (a:b:xs) = do
  weights <- randn a b
  bias    <- randn 1 b
  rest    <- buildNetwork act (b:xs)
  return $ (weights, bias, act):rest
buildNetwork _ _ = return []

forward :: NN -> Input -> Output
forward = (fst .) . flip feedForward

feedForward :: Input -> NN -> (Output, [(Input, Deriv)])
feedForward = mapAccumL activateLayer

activateLayer :: Input -> Layer -> (Output, (Input, Deriv))
activateLayer input (wh, bh, act) = (output, (input, deriv))
  where
    output = cmap (fst act) ((input <> wh + bh))
    deriv  = cmap (snd act) output

backprop :: LearningRate -> Input -> Output -> NN -> NN
backprop lr input y net = backpropLayers lr error inpDs net
   where
     (output, inpDs) = feedForward input net
     error = y - output

mse :: NN -> Input -> Output -> Error
mse net x y = let (output, _) = feedForward x net in y - output

backpropLayers :: LearningRate -> Error -> [(Input, Deriv)] -> NN -> NN
backpropLayers lr error inpDs = snd . mapAccumR (backpropMatrix lr) error . zip inpDs
--  = ((snd . mapAccumR (backpropMatrix lr) error .) . zip) inpDs

backpropMatrix :: LearningRate -> Matrix R -> ((Matrix R, Matrix R), Layer) -> (Error, Layer)
backpropMatrix lr error (inputInfo, layer) = (prev_error, new_layer)
  where
    (layer_weights, layer_bias, act) = layer
    (layer_inp, layer_deriv) = inputInfo
    new_layer = (new_weights, new_bias, act)

    d_layer = layer_deriv * error
    new_weights = (+) layer_weights $ cmap (* lr) $ (tr layer_inp) <> d_layer
    new_bias = (+) layer_bias $ cmap (* lr) $ asRow $ vector $ map sumElements $ toColumns d_layer

    prev_error = d_layer <> (tr layer_weights)
    
sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

tanh' :: Double -> Double
tanh' x = let fx = tanh x in 1 - fx * fx

printNet :: NN -> IO ()
printNet layers = do
  mapM_ (\(w,b,_) -> do
            disp 3 w
            disp 3 b) layers
                 
