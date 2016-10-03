module Simulate where

import Data.Ratio
import Control.Monad

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
