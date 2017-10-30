module Lib
    ( update
    , greedySelect
    , eGreedySelect
    , narmed
    , QTable (..)
    , State
    , Action
    , LearningRate
    , train
    , transition
    , trainGrid
    , greedyIO
    ) where

import qualified Data.Map.Strict as M
import System.Random
import Data.Random.Normal
import System.Random.Shuffle

type Action = Int
type State = Int

type Pair = (State, Action)
type Reward = Double
type LearningRate = Double
type QTable = M.Map Pair Reward


update :: LearningRate -> Pair -> Reward -> QTable -> QTable
update lr pair r table = if M.member pair table
                         then M.update (\_ -> Just new_r) pair table
                         else M.insert pair r table
  where
    old_r = table M.! pair
    new_r = old_r * (1 - lr) + lr * r

greedyIO :: QTable -> State -> IO Action
greedyIO = greedySelect

greedySelect :: QTable -> State -> IO Action
greedySelect table state = do
  gen <- getStdGen
  let allK = M.keys table
  return $ fst $ foldr (\k (a,r) -> let new_r = table M.! k
                                    in if new_r > r
                                       then (snd k, new_r)
                                       else (a,r))
    (1, -1000)
    (filter (\(s,_) -> s == state) (shuffle' allK (length allK) gen))

eGreedySelect :: Double -> QTable -> State -> IO Action
eGreedySelect eps table state = do
  r <- randomIO :: IO Double
  x <- randomRIO (0, length (M.keys table) - 1) :: IO Int
  if r < eps then return $ snd ((M.keys table) !! x)
    else greedySelect table state

narmed :: Action -> IO Reward
narmed 1 = normalIO' (1,    sqrt 5)
narmed 2 = normalIO' (1.5,  sqrt 1)
narmed 3 = normalIO' (2,    sqrt 1)
narmed 4 = normalIO' (2,    sqrt 2)
narmed 5 = normalIO' (1.75, sqrt 10)

gridWorld :: State -> Action -> Reward
gridWorld 39 2 = 100
gridWorld _ _ = -1

transition :: State -> Action -> State
transition s 1 = if s < 10 then s else s - 10
transition s 2 = if s == 9 || s == 19 || s == 29 || s == 39 || s == 49 then s else s + 1
transition s 3 = if s > 39 then s else s + 10
transition s 4 = if s == 0 || s == 10 || s == 20 || s == 30 || s == 40 then s else s - 1
transition s 5 = s

trainGrid
  :: LearningRate
  -> QTable -- current q table
  -> (QTable -> State -> IO Action) -- function to select next action
  -> State -- current state
  -> IO (QTable, State, Reward) -- new qtable, next state, reward from step
trainGrid lr qtable action state = do
  act <- action qtable state
  let next = transition state act
      reward = gridWorld state act
      
  nextR <- greedySelect qtable next
  
  let est_future = gridWorld next nextR
      qtable' = update lr (state, act) (0.9 * est_future + reward) qtable
  return (qtable', next, reward)
  
train :: LearningRate -> QTable -> (QTable -> State -> IO Action) -> IO (QTable, Reward)
train lr qtable action = do
  act <- action qtable 0
  r <- narmed act
  return $ (update lr (0,act) r qtable, r)

  
someFunc :: IO ()
someFunc = putStrLn "someFunc"
