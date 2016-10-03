module Main where

import Data.List
import Data.Ratio
import Control.Monad
import System.Environment

------------------------
-- |Simulation Interface
------------------------

data P a = P [(a, Rational)]
  deriving Show 

instance Functor P where
  fmap  = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return x = P[(x, 1)]
  P xps >>= f = P $ concat ypss
    where ypss = map (\(x, px) -> let P yps = f x in map (\(y, py) -> (y, px * py)) yps) xps

uniform :: [a] -> P a
uniform xs = P $ zip xs $ repeat (1 % (toInteger $ length xs))

pOf :: P Bool -> Rational
pOf (P bps) = foldl (\tp (b, p) -> if b then p + tp else tp) 0 bps

support :: P a -> [a]
support (P aps) = map fst aps

normalize :: Eq a => P a -> P a
normalize (P xs) = P $ foldr ins [] xs
  where ins (y, py) yps = if elem y (map fst yps) 
                          then map (update (y, py)) yps
                          else (y, py) : yps
        update (x, px) (y, py) = if x == y then (y, px + py) else (y, py)

-----------------------
-- |Inference interface
-----------------------

infer :: Eq b => [a] -> P b -> (a -> P b) -> P a
infer range dobserved step = P $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, step x)) range
        match (P xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (P yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps

----------------------------------
-- |Models for probablistic events
----------------------------------

data Die = Die Int
  deriving (Eq, Show)
data Roll = Face Int
  deriving (Eq, Show)
data Flip = H | T
  deriving (Eq, Show)

-----------------------
-- |Simulation Problems
-----------------------
simpleDieNCoins :: P Int
simpleDieNCoins = do
  n <- uniform [1..6]
  fs <- sequence $ replicate n (uniform [H, T])
  let hs = length $ filter (== H) fs
  return hs

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

----------------------
-- |Inference Problems
----------------------

-----------------------------------------------------------
-- |Roll a die for N, flip N coins, see 3 heads, what is N?
-----------------------------------------------------------
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

--------------------------------------
-- |Interactively run invoked problems 
--------------------------------------

main :: IO ()
main = do 
  [prob] <- getArgs
  putStrLn $ "Running problem " ++ prob
  case prob of {("A") -> print $ normalize probD;
                ("diecoins") -> print $ normalize inferDieNCoins;
                (_)  -> error $ "problem " ++ prob ++ " not yet implemented"}
  -- print $ inferDieNCoins
  -- print $ inferDummy
  -- print $ normalize probD
  -- print $ probC1 (uniform [Die 4, Die 6, Die 12, Die 20])
  -- print $ probC2 (uniform [Die 4, Die 6, Die 12, Die 20])
  -- print $ p2d4 (uniform [Die 4, Die 6, Die 12, Die 20])

----------------------
-- Carcass of failures
----------------------

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


-- inferClever :: P Flip
-- inferClever = thing 
--   where thing = (P[(clev1, (1%2)), (clev2, (1%2))]) `ap` P[(3, 1)]
-- 
-- clev1 3 = H
-- clev1 3 = H
-- clev2 2 = T
