{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Enumerable
import Control.Monad

main :: IO ()
main = do print $ normalize simpleDieNCoins 
          print $ inferDieNCoins
          print $ inferDummy

simpleDieNCoins :: P Int
simpleDieNCoins = do
  n <- uniform [1..6]
  fs <- sequence $ replicate n (uniform [H, T])
  let hs = length $ filter (== H) fs
  return hs

infer :: (Enumerable a, Eq b) => (a -> P b) -> P b -> P a
infer f dobserved = P $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, f x)) enumerate
        match (P xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (P yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps

instance Enumerable Roll
  where enumerate = map Face [1..6]

inferDieNCoins :: P Roll
inferDieNCoins = ((\(Face n) -> sequence $ replicate n (uniform [H, T])) >=>
                     (\fs -> let hs = length $ filter (== H) fs
                               in return hs)) `infer` P [(3, 1)]

instance Enumerable Flip
  where enumerate = [H, T]

inferDummy :: P Flip
inferDummy = (\f -> case f of {(H) -> uniform [2]; (T) -> uniform [3]})
             `infer` P [(3, 1)] 
