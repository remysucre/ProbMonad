module Simulations where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Ratio
import Control.Monad

-- simpleDieNCoins :: P Int
-- simpleDieNCoins = do
--   n <- uniform [1..6]
--   fs <- sequence $ replicate n (uniform [H, T])
--   let hs = length $ filter (== H) fs
--   return hs

probD :: P Bool
probD = do
  (d1, d2) <- dDie
  ds <- dSum
  let dd612 = d1 /= d2
  let ds11 = ds == 11
  return $ dd612 && ds11

dDie :: P (Die, Die)
dDie = do 
  d1 <- uniform [Die 6, Die 12]
  d2 <- uniform [Die 6, Die 12]
  return (d1, d2)

dSum :: P Integer
dSum = do
  f1 <- uniform [1..12]
  f2 <- uniform [1..6]
  return (f1 + f2)

p2d :: P Die -> P (Die, Die)
p2d pd = do
  d1 <- pd
  d2 <- pd
  return (d1, d2)

pd6d12 :: P Die -> P Bool
pd6d12 pd = do
  (d1, d2) <- p2d pd
  return $ ((d1 == Die 6) && (d2 == Die 12) || (d2 == Die 6) && (d1 == Die 12))

pdxd20 :: P Die -> P Bool
pdxd20 pd = do
  (d1, d2) <- p2d pd
  return $ ((d1 /= Die 20) && (d2 == Die 20) || (d2 /= Die 20) && (d1 == Die 20))

probC1 :: P Die -> Rational
probC1 pd = pOf $ pd6d12 pd

probC2 :: P Die -> Rational
probC2 pd = pOf $ pdxd20 pd

probF :: Rational
probF = undefined

pds :: P Die -> P [Die]
pds pd = sequence $ replicate 30 pd

p2d4 :: P Die -> Rational
p2d4 pd = pOf (do 
  ds <- pds pd
  return $ 2 == (length $ filter (== Die 4) ds))
