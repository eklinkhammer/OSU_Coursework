module Lib
    ( someFunc
    ) where

import Numeric.LinearAlgebra

u = vector [1..5]

v = vector [10,-3,0,4,5]


someFunc :: IO ()
someFunc = putStrLn $ show (u + v)
