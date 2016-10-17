{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Ratio
import Control.Monad
import System.Environment
import System.Random

main = do
  let bowl = replicate 14 20 ++ replicate 14 12 ++ replicate 9 8 ++ replicate 9 6
  putStrLn $ "A." ++ show probA
  putStrLn $ "B." ++ show (toPercent probB) 
  putStrLn $ "C." ++ show (toPercent $ fst (probC bowl), toPercent $ snd (probC bowl))
  putStrLn $ "D." ++ show (toPercent (probD bowl))
  putStrLn $ "E." ++ show (toPercent (probE bowl))
  putStrLn $ "H." ++ show (toPercent probH)
  putStrLn $ "I." ++ show (toPercent probI)
  putStrLn $ "J." ++ show (toPercent probJ)
  putStrLn $ "M." ++ show (normalize probM)
  putStrLn $ "N." ++ show ((fromRational (probN bowl)) :: Double)
  putStrLn $ "P." ++ show ((fromRational (probP bowl)) :: Double)

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

infer :: Eq b => Dist a -> (b -> Bool) -> (a -> Dist b) -> Dist a
infer (Dist dxPrior) observe likelyhood = 
  Dist $ map (\(x, px) -> (x, px * w x)) dxPrior
  where w x = pOfb (likelyhood x) observe 

normalize :: Eq a => Dist a -> Dist a
normalize (Dist xs) = Dist $ map (\(x, p) -> (x, p / sump)) dedupped
   where sump = sum $ map snd dedupped
         dedupped = foldr ins [] xs
         ins (y, py) yps = if elem y (map fst yps) 
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
-- |Problem B
-------------

probB :: Probability
probB = pOfb 
  (do
     f1 <- throw (Die 6)
     f2 <- throw (Die 12)
     return $ f1 + f2)
  (== 11)

-------------
-- |Problem C
-------------

probC :: [Int] -> (Probability, Probability)
probC bowl = 
  let p612 = pOfb ((,) <$> draw bowl <*> draw bowl)
                  (\(d1, d2) -> 
                      elem (Die 12) [d1, d2] && 
                      elem (Die 6) [d1, d2])
      p20 = pOfb ((,) <$> draw bowl <*> draw bowl)
                 (\(d1, d2) -> 
                      elem (Die 20) [d1, d2] && 
                      (d1 /= d2))
  in (p612, p20)

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
-- |Problem E
-------------

-- infer :: Eq b => Dist a -> (b -> Bool) -> (a -> Dist b) -> Dist a

probE :: [Int] -> Probability
probE bowl = pOfb 
  (infer (do d1 <- draw bowl
             d2 <- draw bowl
             return (d1, d2)) 
        (== 11) 
        (\(d1, d2) -> 
            do
              f1 <- throw d1
              f2 <- throw d2
              return (f1 + f2)))
  (\(d1, d2) -> elem (Die 6) [d1, d2] && elem (Die 12) [d1, d2]) 

-------------
-- |Problem H
-------------

flipC :: Dist Flip
flipC = uniform [H, T]

probH :: Probability
probH = pOfb
  (do f <- throw (Die 6)
      fs <- sequence $ replicate f flipC
      return fs)
  (\fs -> isInfixOf [H, H, H] fs) 

-------------
-- |Problem I
-------------

probI :: Probability
probI = pOfb dFacePost (== 3)
  where dFacePost =
          infer (throw $ Die 6)
                (\fs -> (length $ filter (== H) fs) == 3)
                (\n -> sequence $ replicate n flipC)

-------------
-- |Problem J
-------------

probJ :: Probability
probJ = pOfb dfs (\fs -> (length $ filter (== H) fs) == 3)
  where dfs =
         (do f <- throw (Die 6)
             if f > 4 then sequence $ replicate f flipC
                      else uniform [])

-------------
-- |Problem M
-------------
probM = infer (throw $ Die 6)
              (\fs -> (length $ filter (== H) fs) == 3)
              (\n -> sequence $ replicate n flipC)

-------------
-- |Problem N
-------------

probN :: [Int] -> Probability
probN bowl = pOfb (eColr . uniform $ map Die bowl) (== 3)

eColr :: Dist Die -> Dist Int
eColr bowl = do 
  Die d1 <- bowl
  Die d2 <- bowl
  let throws = 
        (replicate 3  (let f1 = (uniform [1..d1]);
                           f2 = (uniform [1..d2]);
                           sum = (+) <$> f1 <*> f2
                           mark = (> 16) <$> sum
                       in mark))
  rightMark <- foldr (\dm dn -> normalize $ (do m <- dm
                                                n <- dn
                                                if m then return $ n + 1
                                                     else return n))
                     (return 0)
                     throws
  return rightMark
  


-------------
-- |Problem P
-------------

probP :: [Int] -> Probability
probP ds = 30 * expectation (\b -> if b then 1 else 0) eColTest
          where eColTest = eColR (uniform $ map Die ds)

eColR :: Exp Die -> Exp Bool
eColR bowl = do
  Die d1 <- bowl
  Die d2 <- bowl
  f1 <- uniform [1..d1]
  f2 <- uniform [1..d2]
  return $ f1 + f2 > 16
