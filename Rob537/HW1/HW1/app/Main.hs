{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Numeric.LinearAlgebra
import Data.Traversable

train_1_path = "/Users/klinkhae/OSU_Coursework/Rob537/HW1/hw1_data/train1.csv"
train_2_path = "/Users/klinkhae/OSU_Coursework/Rob537/HW1/hw1_data/train2.csv"
test_1_path = "/Users/klinkhae/OSU_Coursework/Rob537/HW1/hw1_data/test1.csv"
test_2_path = "/Users/klinkhae/OSU_Coursework/Rob537/HW1/hw1_data/test2.csv"
test_3_path = "/Users/klinkhae/OSU_Coursework/Rob537/HW1/hw1_data/test3.csv"

lrs = [0.005, 0.01, 0.05, 0.1]
hus = [4, 8, 12, 16]
eps = [1000, 5000, 10000, 100000]
sig = [True, False]

main :: IO ()
main = do
  let exps = generateExps lrs eps hus sig
  
  putStrLn "All Experiments with training data set 1"
  runExpsIO train_1_path [train_1_path, test_1_path, test_2_path, test_3_path] exps

  putStrLn "All Experiments with training data set 2"
  runExpsIO train_2_path [train_2_path, test_1_path, test_2_path, test_3_path] exps
