module Inferences where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Ratio
import Control.Monad

inferDieNCoins :: P Roll
inferDieNCoins = infer (map Face [1..6]) 
                       (P [(3, 1)])
                       (\(Face n) 
                       -> (sequence $ replicate n (uniform [H, T])) >>= \fs 
                       -> let hs = length $ filter (== H) fs
                           in return hs)

inferDummy :: P Flip
inferDummy = infer [H, T] 
                   (P [(3, 1)]) 
                   (\f -> case f of H -> uniform [1]
                                    T -> uniform [3])

-- probE :: P Die -> Rational
-- probE = 
-- 
-- pDie :: P Die
-- 
-- simulate :: P Int
-- simulate = do
--   d1 <- pDie
--   d2 <- pDie
-- 
