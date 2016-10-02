module Inferences where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Enumerable
import Data.Ratio
import Control.Monad

instance Enumerable Roll
  where enumerate = map Face [1..6]

inferDieNCoins :: P Roll
inferDieNCoins = (\(Face n) -> (sequence $ replicate n (uniform [H, T])) >>=
                     \fs -> let hs = length $ filter (== H) fs
                               in return hs) `infer` P [(3, 1)]

instance Enumerable Flip
  where enumerate = [H, T]

inferDummy :: P Flip
inferDummy = (\f -> case f of {(H) -> uniform [2]; (T) -> uniform [3]})
             `infer` P [(3, 1)] 
