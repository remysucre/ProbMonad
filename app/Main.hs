-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE GADTs #-}

module Main where

import Simulate
import Simulations
import Inferences


main :: IO ()
main = do print $ normalize simpleDieNCoins 
          print $ inferDieNCoins
          print $ inferDummy


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
