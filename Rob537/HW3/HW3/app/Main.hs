module Main where

import Lib
import qualified Data.Map.Strict as M
import System.Random
import Data.List
import Control.Monad
import Data.List.Split

rep :: Double -> Int -> QTable -> IO (QTable, Double)
rep s 0 t = return (t,s)
rep s n t = do
  (t',s') <- train 0.1 t greedyIO
  putStrLn $ show s'
  rep (s + s') (n - 1) t'

type ActSelect = (QTable -> State -> IO Action)

repF :: Int -> QTable -> ActSelect -> IO [Double]
repF n t f = do
  (_, rs) <- repHelper 0 n t f
  return (0:rs)

trainQ :: Int -> Int -> QTable -> ActSelect -> IO (QTable, [Double])
trainQ 1 numS t f = do
  (t', total) <- repFG numS t f
  return (t', [total])
trainQ numE numS t f = do
  (t', total) <- repFG numS t f
  (tf, totals) <- trainQ (numE - 1) numS t' f
  return (tf, total : totals)
  
repFG :: Int -> QTable -> ActSelect -> IO (QTable, Double)
repFG n t f = do
  s <- randomRIO (0,49)
  (t, _, rs) <- repGHelper 0 n f t s
  return (t, last rs)
  
repHelper
  :: Double
  -> Int -> QTable -> (QTable -> State -> IO Action) -> IO (QTable, [Double])
repHelper s 0 t _ = return (t, [])
repHelper s n t f = do
  (t', r) <- train 0.1 t f
  (table, vals) <- repHelper (s + r) (n - 1) t' f
  return (table, (s + r) : vals)

repGHelper :: Double -> Int -> ActSelect -> QTable -> State -> IO (QTable, State, [Double])
repGHelper s 0 _ t st = return (t, st, [])
repGHelper s n f t st = do
  (t', st', r) <- trainGrid 0.05 t f st
  if r == 100 then return (t', st', [r + s])
    else do
    (table, next, vals) <- repGHelper (s + r) (n - 1) f t' st'
    return (table, next, (min 100 (s + r)) : vals)
  
averageLists :: [[Double]] -> [Double]
averageLists = map mean . transpose

mean :: [Double] -> Double
mean xs = sum xs / genericLength xs

windowAvg :: Int -> [Double] -> [Double]
windowAvg n xs = map mean (chunksOf n xs)
--repGridHelper
--  :: Double -> Int -> QTable -> ActSelect -> 
main = do
  -- N-Armed Bandit
  let actions = [(0,1), (0,2), (0,3), (0,4), (0,5)]
      qtable = foldr (\x table -> update 0.1 x 1 table) M.empty actions
      newTable = update 0.1 (actions !! 0) 2 qtable

  -- Gridworld
 -- start <- randomRIO (0, 49)
  let gActions = [1,2,3,4,5]
      states = [0..49]
      stateAction = [(s,a) | s <- states, a <- gActions]
      qtableG = foldr (\x t -> update 0.1 x 0 t) M.empty stateAction
      
  --putStrLn $ show newTable
  --putStrLn $ show (greedySelect newTable 0)
  
  -- putStrLn $ show t

  trials_ng <- replicateM 1000 (repF 10 qtable greedyIO)
  trials_ne <- replicateM 1000 (repF 10 qtable (eGreedySelect 0.1))


  result <- replicateM 1000 (trainQ 1000 20 qtableG (eGreedySelect 0.25))
  let rewards = map snd result
      avgGroup = map (windowAvg 50) rewards
  putStrLn $ show (averageLists avgGroup)

  putStrLn $ show (averageLists trials_ng)
  putStrLn $ show (averageLists trials_ne)
