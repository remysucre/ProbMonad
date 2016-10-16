{-# LANGUAGE TypeSynonymInstances #-}

module Inf where

import Data.Ratio
import Control.Monad
import Control.Monad.Fix

type Prob = Rational
type Face = Int

class Monad m => ProbabilityMonad m where
  choose :: Prob -> m a -> m a -> m a
  uniform :: [a] -> m a
  uniform l@(x:xs) = foldl (\dys y -> choose p (return y) dys) (return x) l
    where p = 1 % (fromIntegral . length) l
  {-# MINIMAL (choose) #-}

--------------------------------------------------------------
-- |Distribution as a function mapping events to probabilities
--------------------------------------------------------------


newtype Dist a = Dist (a -> Prob)

instance Functor Dist where
  fmap  = liftM

instance Applicative Dist where
  pure = return
  (<*>) = ap

instance Monad Dist where
  return x = Dist $ \y -> if y == x then 1 else 0
  (Dist dx) >>= f = Dist $ \y -> \x -> dx x * let Dist dy = f x in dy y



type Inf a b = b -> Dist a

-- | infer da @ [prior dist of a] lb @ [likelyhood of b from a]
infer :: Dist Face -> (Face -> Dist b) -> Inf Face b
-- [F] -> [b] -> (F -> Prob) -> (F -> (b -> Prob)) -> (b -> (F-> Prob))
infer Dist da lb = \b a -> (*) <$> (lb a <$> b) <*> (da <$> a)
-- infer da lb = inf
--   where inf x = Dist (\y -> (*) <$> (lb a <$> b) <*> (Dist da a))
-- (\x -> p)
-- (\x -> Dist y)

-- instance ProbabilityMonad Dist where
--   choose p (Dist dx) (Dist dy) = Dist $ dx' ++ dy'
--     where dx' = map (\(x, px) -> (x, px*p)) dx
--           dy' = map (\(y, py) -> (y, py*(1-p))) dy
--   uniform xs = Dist $ zip xs $ repeat (1 % (toInteger $ length xs))
-- 
-- -- | E1. infers a posterior distribution of faces after observing the face
-- fid :: Inf Face Face
-- fid x = \y -> if y == x then 1 else 0
-- 
-- -- | E2. an example of an Inf type, infers a distribution of faces from an 
-- -- observation of bool, where bool is whether the face <= 3
-- small :: Inf Face Bool
-- small True = \x -> if x <= 3 then 1%3 else 0
-- small False = \x -> if x > 3 then 1%3 else 0
-- 
-- -- posterior probability of throwing 3 from the observation that the throw is
-- -- less than 3 (the predicate face <= 3 is True)
-- p2Gt :: Prob
-- p2Gt = small True 3
-- 
-- -- | infers a posterior distribution of faces from the likely hood of marking
-- -- the left (<= 3) column
-- mark3 :: Inf Face Bool
-- mark3 = infer d6 mark
-- 
-- -- distribution of faces on a d6
-- d6 :: Dist Face
-- d6 n = if elem n [1..6] then 1%6 else 0
-- 
-- -- likelyhood of marking the left col from each face
-- mark :: Face -> Dist Bool
-- mark a = \b -> if a <= 3 then (if b then 1 else 0)
--                          else (if b then 0 else 1)
-- 
-- -- | homework problems
-- 
-- -- | A. What distribution of integers results from throwing a single
-- -- d6? What about a d12?
-- 
-- d6f :: Dist Int
-- d6f = undefined
