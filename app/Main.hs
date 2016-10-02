-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE GADTs #-}

module Main where

import Gamble
import Simulate
import Infer

import Data.List
import Data.Enumerable
import Data.Ratio
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

-- data Inf a = Inf (P (a, Roll))

-- infers :: P a -> P Roll
-- infers = undefined

-- instance Functor Inf where
--   fmap  = liftM
-- 
-- instance Applicative Inf where
--   pure = return
--   (<*>) = ap

-- instance Monad Inf
--    where 
--          return x = Inf . P $ map (\n -> ((x, Face n), 1)) [1..6] 
--          -- (>>=) :: Inf a -> a -> Inf b -> Inf b 
--          Inf (P xps) >>= f = Inf . P $ concat ypss
--             where ypss = map (\((x, r), px) -> let Inf (P yps) = f x in map (\(y, py) -> (y, px * py)) yps) xps
--           -- TODO throwing away r here, troublesome

-- coolInfer ::  Inf Int
-- coolInfer = do 
--   (\n -> sequence (replicate n uniform [H, T]) >>= 
--       \fs -> return length $ filter (== H) fs) [1..6] 

infer :: (Enumerable a, Eq b) => (a -> P b) -> P b -> P a
infer f dobserved = P $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, f x)) enumerate
        match (P xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (P yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps

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

-- inferClever :: P Flip
-- inferClever = thing 
--   where thing = (P[(clev1, (1%2)), (clev2, (1%2))]) `ap` P[(3, 1)]
-- 
-- clev1 3 = H
-- clev1 3 = H
-- clev2 2 = T
