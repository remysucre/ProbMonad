module Main where

import Simulate
import Gamble
import Simulations
import Inferences


main :: IO ()
main = do 
  print $ inferDieNCoins
  print $ inferDummy
  print $ normalize probD
  print $ probC1 (uniform [Die 4, Die 6, Die 12, Die 20])
  print $ probC2 (uniform [Die 4, Die 6, Die 12, Die 20])
  print $ p2d4 (uniform [Die 4, Die 6, Die 12, Die 20])
