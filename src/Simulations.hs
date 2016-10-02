module Simulations where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Enumerable
import Data.Ratio
import Control.Monad

simpleDieNCoins :: P Int
simpleDieNCoins = do
  n <- uniform [1..6]
  fs <- sequence $ replicate n (uniform [H, T])
  let hs = length $ filter (== H) fs
  return hs
