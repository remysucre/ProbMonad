module Infer where

import Simulate

infer :: Eq b => [a] -> P b -> (a -> P b) -> P a
infer range dobserved f = P $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, f x)) range
        match (P xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (P yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps
