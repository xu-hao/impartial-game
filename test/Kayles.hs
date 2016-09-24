{-# LANGUAGE FlexibleInstances, TypeFamilies, FlexibleContexts, UndecidableInstances, TypeApplications, MultiParamTypeClasses #-}

module Main where

import Data.Game.Impartial
import Data.Semigroup
import System.Environment
import Data.Coerce (coerce)

instance Position (BasePosition Int) where
    type Next (BasePosition Int) = Simple (SumPosition (BasePosition Int))
    next = coerce testWin

testWin :: Int -> [[Int]]
testWin n = map (\offset -> testWinIndex n offset 2) [0..(n-2) `div` 1] ++ map (\offset -> testWinIndex n offset 1) [0..(n-1) `div` 1]

testWinIndex :: Int -> Int -> Int -> [Int]
testWinIndex i offset remove =
      [offset, i - offset - remove]

main :: IO ()
main = do
    args <- getArgs
    let v = nimber (coerce (map read args :: [Int]) :: Normal (Simple (SumPosition (BasePosition Int))))
    print v
    print (v > 0)
