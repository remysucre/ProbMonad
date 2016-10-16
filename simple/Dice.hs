module Dice where

import Data.List
import Data.Ratio
import Control.Monad
import System.Environment
import System.Random

-- main = print $ expectation fromIntegral eColTest 
-- main = do 
--   putStrLn "Which problem to run? [C G diencoins]"
--   p <- getLine
--   case p of {("C") -> pc;
--              ("G") -> pg;
--              ("diencoins") -> dnc;
--              (_)  -> putStrLn $ "Problem " ++ p ++ " not yet implemented"}

pg :: IO()
pg = do
  putStrLn "What's in the bowl? for example, [4, 6, 6, 12]"
  ds <- read <$> getLine
  putStrLn "Expected marks on the right column is"
  print $ probG ds

pc :: IO()
pc = do
  putStrLn "What's in the bowl? for example, [4, 6, 6, 12]"
  ds <- read <$> getLine
  let (p1, p2) = probC ds
  putStrLn $ "Probability of drawing a d6 and a d12 is " ++ show p1
  putStrLn $ "Probability of drawing only one d20 is " ++ show p2

dnc :: IO()
dnc = do
  putStrLn "throw a d6 for N, flip a coin N times, see 3 heads. Then the distribution of faces is: "
  print $ normalize simpleDieNCoins

--------------------------
-- |Probablistic Interface
--------------------------

type Probability = Rational

class Monad m => ProbabilityMonad m where
  choose :: Probability -> m a -> m a -> m a
  uniform :: [a] -> m a
  uniform l@(x:xs) = foldl (\dys y -> choose p (return y) dys) (return x) l
    where p = 1 % (fromIntegral . length) l
  {-# MINIMAL (choose) #-}

class ProbabilityMonad m => SupportMonad m where
  support :: m a -> [a] 
-- support (return x) = [x] 
-- support (d >>= k) =
--   concat [support (k x) | x <- support d]
-- support (choose p d d') =
--   support d ++ support d'

class ProbabilityMonad m => ExpMonad m where
  expectation :: (a -> Rational) -> m a -> Rational
-- expectation h (return x) = h x
-- expectation h (d >>= k) = expectation g d
--    where g x = expectation h (k x)
-- expectation h (choose p d d') =
--    p * expectation h d +
--    (1-p) * expectation h d'

class ProbabilityMonad m => SamplingMonad m where
  sample :: RandomGen g => m a -> g -> (a, g)
-- sample (return x) g = (x, g)
-- sample (d >>= k) g =
--   let (x, g') = sample d g in sample (k x) g'
-- sample (choose p d d') g =
--   let (x, g') = random g in
--   sample (if x < p then d else d') g'

------------------------
-- |Simulation Interface
------------------------

----------------------------------------------------------------------
-- |Expectation encodes everything we need to know from a distribution
----------------------------------------------------------------------

newtype Exp a = Exp ((a -> Rational) -> Rational)

instance Functor Exp where
  fmap  = liftM

instance Applicative Exp where
  pure = return
  (<*>) = ap

instance Monad Exp where
  return x = Exp (\h -> h x)
  (Exp d) >>= k = 
     Exp (\h -> let apply (Exp f) arg = f arg
                    g x = apply (k x) h
                in d g)

instance ProbabilityMonad Exp where
  choose p (Exp d1) (Exp d2) =
    Exp (\h -> p * d1 h + (1-p) * d2 h)

instance ExpMonad Exp where
  expectation h (Exp d) = d h

-------------------------------------------------------------
-- |Distribution as association list of event and probability
-------------------------------------------------------------

newtype Dist a = Dist [(a, Probability)] 

toPercent :: Rational -> Double
toPercent r = 100 * fromRational r

instance Show a => Show (Dist a) where
  show (Dist xps) = concatMap (\(x, p) -> show x 
                                       ++ ":" 
                                       ++ show (toPercent p) 
                                       ++ "%\n")
                              xps

instance Functor Dist where
  fmap  = liftM

instance Applicative Dist where
  pure = return
  (<*>) = ap

instance Monad Dist where
  return x = Dist [(x, 1)]
  Dist dx >>= dyx = 
    Dist $ concat dys
      where dys = map (\(x, px) -> let Dist dy = dyx x 
                                   in map (\(y, py) -> (y, px * py)) dy) 
                      dx

instance ProbabilityMonad Dist where
  choose p (Dist dx) (Dist dy) = Dist $ dx' ++ dy'
    where dx' = map (\(x, px) -> (x, px*p)) dx
          dy' = map (\(y, py) -> (y, py*(1-p))) dy
  uniform xs = Dist $ zip xs $ repeat (1 % (toInteger $ length xs))

-----------------------
-- |Inference Interface
-----------------------

infer :: Eq b => [a] -> Dist b -> (a -> Dist b) -> Dist a
infer range dobserved f = Dist $ map (\(a, db) -> (a, match db dobserved)) daprior
  where daprior = map (\x -> (x, f x)) range
        match (Dist xps) dob = foldr (\(x, px) px' ->  plookup x dob * px + px') 0 xps
        plookup x (Dist yps) = foldr (\(y, py) px -> if x == y then py + px else px) 0 yps

simpleDieNCoins :: Dist Int
simpleDieNCoins = do
  n <- uniform [1..6]
  fs <- sequence $ replicate n (uniform [H, T])
  let hs = length $ filter (== H) fs
  return hs

normalize :: Eq a => Dist a -> Dist a
normalize (Dist xs) = Dist $ foldr ins [] xs
  where ins (y, py) yps = if elem y (map fst yps) 
                          then map (update (y, py)) yps
                          else (y, py) : yps
        update (x, px) (y, py) = if x == y then (y, px + py) else (y, py)
-------------------------------------------
-- |Unifinished Monadic Inference Interface
-------------------------------------------

data Inf a b = Inf (b, Dist a)

instance Functor (Inf a) where
  fmap  = liftM

instance Applicative (Inf a) where
  pure = return
  (<*>) = ap

instance Monad (Inf a) where
  return x = Inf (x, Dist [])
  (>>=) = undefined

-----------------------
-- |Probablistic events
-----------------------

--------------------------------------
-- |Deriving Eq to look up probability
--------------------------------------

newtype Die = Die Int
  deriving (Eq, Show)
newtype Roll = Face Int
  deriving (Eq, Show)
data Flip = H | T
  deriving (Eq, Show)

------------------------
-- |Solution to Problems
------------------------

-------------
-- |Problem A
-------------

probA :: (Dist Int, Dist Int)
probA = (uniform [1..6], uniform [1..12])

-------------
-- |Problem C
-------------

pOf :: Dist Bool -> Rational
pOf (Dist bps) = foldl (\tp (b, p) -> if b then p + tp else tp) 0 bps

probC :: [Int] -> (Probability, Probability)
probC ds = (probC1 dd, probC2 dd)
  where dd = (uniform $ map Die ds)

probC1 :: Dist Die -> Probability
probC1 pd = pOf $ pd6d12 pd

probC2 :: Dist Die -> Probability
probC2 pd = pOf $ pdxd20 pd

p2d :: Dist Die -> Dist (Die, Die)
p2d pd = do
  d1 <- pd
  d2 <- pd
  return (d1, d2)

pd6d12 :: Dist Die -> Dist Bool
pd6d12 pd = do
  (d1, d2) <- p2d pd
  return $ ((d1 == Die 6) && (d2 == Die 12) || (d2 == Die 6) && (d1 == Die 12))

pdxd20 :: Dist Die -> Dist Bool
pdxd20 pd = do
  (d1, d2) <- p2d pd
  return $ ((d1 /= Die 20) && (d2 == Die 20) || (d2 /= Die 20) && (d1 == Die 20))

-------------
-- |Problem D
-------------

probD :: [Int] -> Probability
probD bowl = pOfb 
  (do 
     d1 <- draw bowl
     d2 <- draw bowl
     f1 <- throw d1
     f2 <- throw d2
     return (d1, d2, f1 + f2))
  (\res -> res == (Die 6, Die 12, 11) || res == (Die 12, Die 6, 11))

draw :: [Int] -> Dist Die
draw bowl = uniform $ map Die bowl

throw :: Die -> Dist Int
throw (Die d) = uniform $ [1..d]

pOfb :: Dist a -> (a -> Bool) -> Probability
pOfb (Dist dx) f = foldr (\(x, px) p -> if f x then p + px else p) 0 dx / sum (map snd dx)

-------------
-- |Problem G
-------------

probG :: [Int] -> Probability
probG ds = 30 * expectation (\b -> if b then 1 else 0) eColTest
          where eColTest = eColR (uniform $ map Die ds)

eColR :: Exp Die -> Exp Bool
eColR bowl = do
  Die d1 <- bowl
  Die d2 <- bowl
  let f1 = uniform [1..d1]
  let f2 = uniform [1..d2]
  m <- (\m n -> (m + n) >= 8) <$> f1 <*> f2
  return m
