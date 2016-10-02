module Infer where

import Data.Enumerable

import Simulate

infer :: (Enumerable a, Eq b) => (a -> P b) -> P b -> P a
infer f dobserved = P $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, f x)) enumerate
        match (P xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (P yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps
